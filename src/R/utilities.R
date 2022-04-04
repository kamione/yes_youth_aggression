# remove all registered parallel workers
unregister <- function() {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name=env), pos = env)
}