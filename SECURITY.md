# Security Policy

## Supported versions

Security fixes are applied to the current development branch and most recent
released minor version.

## Reporting a vulnerability

Do not open a public issue for a suspected vulnerability or accidental secret
exposure. Email Matteo Mazzarelli at matteo.mazzarelli@gmail.com with:

- affected version and platform;
- reproduction steps or a minimal example;
- potential impact;
- any suggested mitigation.

Please omit real API keys, personal data, and proprietary model artifacts. You
should receive an acknowledgement within seven days.

## Data and network boundaries

- Core explanation audits are local and do not access the network.
- Remote narrative generation is optional and sends only aggregated
  diagnostics. Users remain responsible for provider terms and data policy.
- API keys are read from arguments or environment variables and must never be
  written to reports, logs, fixtures, or error messages.
- Generated HTML escapes model labels, feature names, findings, and metadata.
