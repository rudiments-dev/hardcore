FROM node:11.10.1-alpine as develop-stage
WORKDIR /app
COPY package*.json ./
RUN npm cache clean -f
RUN rm -rf node_modules package-lock.json && npm install
RUN yarn install
COPY . .

EXPOSE 3000
CMD ["yarn", "serve"]