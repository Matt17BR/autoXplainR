# Evidence fingerprint memory check

A wide million-row run finished fitting and scoring but failed before returning
its result: the final evidence fingerprint could not allocate the full serialized
buffer. This probe isolates that failure mode. It does not measure model fitting,
report rendering or a complete `autoxplain()` call.

## Contract and implementation

The identity remains `axr-` followed by the SHA-256 hash of the complete value's
binary version-2 R serialization, excluding the first 14 bytes. No rows, model
state, attributes or reference bindings are omitted. Serialization still defines
the treatment of shared and cyclic references.

The file path serializes to a binary connection in a private temporary directory,
closes the connection, and hashes the file with `serialize = FALSE, skip = 14L`.
It removes its directory after success or failure. This avoids allocating one
raw vector large enough to contain the entire serialization. It requires enough
temporary storage for those bytes; it does not remove the cost of reading and
hashing them. A temporary directory on a memory-backed filesystem still uses that
filesystem's memory.

Ordinary small values retain the original in-memory path. The planner chooses a
file for values estimated at 64 MiB or more, reference-bearing values, and
structures exceeding its 2,048-node inspection budget. It does not enter
environments or visit individual atomic elements. Structural inspection bypasses
class-defined length and extraction methods. The size estimate is not a bound on
serialized length. A narrow fallback also retries recognized English R allocation
errors through the file path; that fallback alone is not portable to every locale.

The compatibility reference is the original `content_fingerprint()` from source
`afefdbe`, frozen as `evidence_contract-before.R` with SHA-256
`267d671af3246dc631b258361b1ee4a40e48bf3cb25810d06f1b3a9b2981ab18`.
The upstream behavior was checked against the package's minimum supported
[digest 0.6.31 R implementation](https://github.com/cran/digest/blob/0.6.31/R/digest.R)
and the measured
[digest 0.6.39 implementation](https://github.com/cran/digest/blob/0.6.39/R/digest.R).
The [file hashing implementation](https://github.com/cran/digest/blob/0.6.31/src/digest.c)
uses incremental reads rather than a full raw serialization buffer.

## Correctness gates

`tests/testthat/test-fingerprint-streaming.R` compares ordinary and file identities
against both the original `digest()` call and independently produced, manually
header-stripped raw serialization. Fixtures include missing values, signed zeros,
non-finite numbers, encodings, factors, dates, POSIXlt, calls with omitted
arguments, function formals, closures and shared cyclic environments. Separate
tests require identities to change for final values, row order, metadata, factor
levels and captured state.

The tests also exercise RNG preservation, class-method side effects, bounded
structural inspection, and connection and temporary-file cleanup after injected
serialization and hashing failures. The combined fingerprint, evidence-contract
and evaluation-snapshot run passed 295 assertions with no failures, warnings or
skips. An independent review checked another 28 assertions, including actual
directory permissions and a file-open failure. Its verdict records the exact
reviewed source hash in `independent-review.json`. Its separate reproduction script
takes the repository and an existing output directory:

```sh
Rscript validation/scalability/fingerprinting/independent-review.R /path/to/repository /path/to/output
```

## Bounded memory experiment

`probe.R` places the same materialized numeric vector in eight list entries.
Version-2 serialization repeats the ordinary vector values, so a 64 MiB vector
produces a serialization slightly larger than 512 MiB. Before and after cases run
in separate R processes under the same 512 MiB address-space limit. The comparison
changes only the fingerprint implementation, not the supplied value or contract.

The candidate also writes an independent oracle file with `base::serialize()`.
The Python supervisor checks the version header, hashes every remaining byte with
`hashlib.sha256`, and compares the result exactly. It independently changes the
last serialized double and verifies the candidate detects that final-value change.
The oracle file is removed after verification. Original fingerprint timing excludes
fixture construction and oracle generation; peak RSS covers the full R process,
including oracle generation and the changed-value check where performed.

Run a 1 MiB sanity case first, then the larger before and after cases. Use a fresh
output directory for every run. The supervisor uses the shared scalability lock,
a 120-second external bound, one native thread, and frozen source copies with
SHA-256 identifiers:

```sh
python3 validation/scalability/fingerprinting/supervise.py \
  --source /path/to/evidence_contract-before.R --variant before \
  --output /path/to/new-before-directory
python3 validation/scalability/fingerprinting/supervise.py \
  --source R/evidence_contract.R --variant after \
  --output /path/to/new-after-directory
```

These probes are a targeted allocation check. A successful complete million-row
public call must be established separately with the final installed package.

## Observed result

All four bounded processes completed. The small before and after fingerprints
were identical. With the 64 MiB vector repeated eight times, the original function
reported `cannot allocate buffer`; the file path returned successfully under the
same 512 MiB address-space limit. The candidate fingerprint took 2.466 seconds and
the full R process reached 198,896 KiB peak RSS.

The independent Python hash matched the 536,870,998-byte serialized file after
excluding its 14-byte header, and also matched the fingerprint after changing the
final numeric value. Temporary directories were removed and RNG state was unchanged. Exact
fingerprints, source identifiers, timings, bounds and both independent byte
verdicts are in `measurements.json`. These are single observations on one host;
they show the allocation failure is avoidable, not a general speedup estimate.

After measurement, lint required continuation indentation and braces around one
scalar `else 0L` branch. Those are the only changes to the measured runtime file;
the focused fingerprint and uncertainty suites then passed 257 assertions. The
full source suite had passed 5,293 assertions before those style changes. The
measurement record retains both source hashes and this exact distinction.
