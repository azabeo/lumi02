library(logging)
logging::basicConfig('DEBUG')
logging::basicConfig()
addHandler(writeToFile, logger="company", file="log/sample.log")
loginfo("hello world", logger="")
logwarn("hello company", logger="company")
loginfo("ciao")
logdebug('ciao')