FROM nginx:latest

COPY static_content /usr/share/nginx/html

COPY nginx.conf /etc/nginx/nginx.conf
EXPOSE 80