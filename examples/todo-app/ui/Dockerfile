FROM node:11.10.1-alpine as develop-stage
WORKDIR /app
COPY package*.json ./
RUN npm cache clean -f
RUN rm -rf node_modules package-lock.json && npm install
RUN yarn install --mode docker
COPY . .

FROM develop-stage as build-stage
RUN yarn build --mode docker

FROM nginx:1.15.7-alpine as production-stage
COPY --from=build-stage /app/dist /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]