## Start Coding

From root directory, install dependencies with

```
raco pkg install src
raco pkg install https://github.com/n3mo/data-science.git
```


if its package is already intalled, update the dependencies

`raco pkg update --link src`


### Coverage
To execute coverage command, run:
```bash
raco cover -f html src
```
