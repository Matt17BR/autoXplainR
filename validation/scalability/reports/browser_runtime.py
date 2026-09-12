"""Normal pinned Playwright launches, with an optional isolated Linux ABI shim."""
import os
from pathlib import Path


def launch(runtime, engine):
    options = {"timeout": 20000}
    # CI uses supported Ubuntu and the ordinary Playwright dependency install.
    # This opt-in path lets a newer Ubuntu host run that same WebKit binary
    # against compatible libraries extracted into a cache, without replacing
    # system libraries or changing any user browser profile.
    bundle = os.environ.get("AXR_WEBKIT_BUNDLE")
    libraries = os.environ.get("AXR_BROWSER_COMPAT_LIBS")
    if engine == "webkit" and bundle and libraries:
        root = Path(bundle).resolve() / "minibrowser-wpe"
        options["executable_path"] = str(root / "bin/MiniBrowser")
        options["env"] = os.environ | {
            "WEBKIT_EXEC_PATH": str(root / "bin"),
            "WEBKIT_INJECTED_BUNDLE_PATH": str(root / "lib"),
            "WEBKIT_INSPECTOR_RESOURCES_PATH": str(root / "share"),
            "LD_LIBRARY_PATH": ":".join([libraries, str(root / "lib"), str(root / "sys/lib")]),
        }
    return getattr(runtime, engine).launch(**options)
