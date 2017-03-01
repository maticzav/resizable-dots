FROM node:7.2.0

MAINTAINER Matic Zavadlal <matic.zavadlal@gmail.com>

# Install elm
RUN npm install -g elm@0.18.0
RUN npm install -g elm-test@0.18.0

# Create app directory
RUN mkdir /app/
WORKDIR /app

# Install elm modules
COPY elm-package.json /app/
RUN elm package install -y

RUN ls elm-stuff/

# Install npm dependencies
COPY package.json /app/
RUN npm install

# Bundle source
COPY . /app

# Build
RUN elm make src/ResizableDots.elm --output build/index.html

# Start
EXPOSE 3000
ENV PORT=3000
ENTRYPOINT ["npm","run","start"]
