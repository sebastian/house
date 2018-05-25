FROM arm32v7/debian:latest

RUN bash -c "apt-get update"
RUN bash -c "apt-get install -y curl build-essential llvm-3.9 llvm-3.9-dev llvm-3.9-runtime llvm-3.9-runtime llvm-3.9-tools libllvm3.9"
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"
RUN bash -c "echo \"export=PATH=\\\$PATH:/root/.local/bin\" >> /root/.bashrc"
