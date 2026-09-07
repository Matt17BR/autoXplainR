# Extract the current release section, retaining intentional Markdown/newlines.
version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
lines <- readLines("NEWS.md", warn = FALSE)
heading <- paste("# AutoXplainR", version)
stopifnot(identical(lines[[1L]], heading))
next_heading <- which(grepl("^# AutoXplainR ", lines))
end <- if (length(next_heading) > 1L) next_heading[[2L]] - 1L else length(lines)
dir.create("release", showWarnings = FALSE)
writeLines(lines[seq_len(end)], "release/release-notes.md")
