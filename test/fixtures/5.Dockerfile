# this will prevent the pragma from taking effect
#syntax=docker/dockerfile:1.0
# escape = `

FROM debian:10

RUN first cmd \
 && second cmd
