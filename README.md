# svn-multi-commit.el

Simple command to handle commit on multiple SVN branches at the same
time.

This code is currently very simple, feel free to suggest enhancements.

## Usage

In each branch's vc-mode, mark the files you want to commit and run
`M-x svn-multi-commit-add`.

When you have added all your file to the commit list, run `M-x
svn-multi-commit-do`.

A new buffer will open to write your commit message. Hit `C-c C-c` to commit when you are done, or `C-c C-k` to cancel the commit.

## License

This code is licensed under the MIT License.
