# svn-multi-commit.el

Simple command to handle commit on multiple SVN branches at the same
time.

This code is currently very simple, feel free to suggest enhancements.

## Usage

In each branch's vc-mode, mark the files you want to commit and run
`M-x svn-multi-commit-add`.

When you have added all your file to the commit list, run `M-x
svn-multi-commit-do`.

You will be asked to confirme you want to commit *these* files, And
then you can enter your commit message.

## License

This code is licensed under the MIT License.
