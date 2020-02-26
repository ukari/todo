# todo

## usage
```
Usage: todo [-s|--source FILE] COMMAND

Available options:
  -s,--source FILE         Specify a storage file
  -h,--help                Show this help text

Available commands:
  add                      Add a todo task
  list                     List all unfinished tasks with index
  fine                     Finish a task specify by index
  gc                       Collect garbage, which would clean all unused history
  version                  Print version

```

## auto completion

- for `bash`

``` bash
todo --bash-completion-script `which todo`| sudo tee /etc/bash_completion.d/todo
```

- for `zsh` use `--zsh-completion-script`
- for `fish` sue `--fish-completion-script`
