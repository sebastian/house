docker run -it --rm \
  -v $PWD:/root/source \
  -v $PWD/.docker-cache/.stack:/root/.stack \
  house:latest /bin/bash -c "cd /root/source && stack build"
