FROM onedata/fedora-systemd:23
MAINTAINER Krzysztof Trzepla <krzysztof.trzepla@gmail.com>

# Build arguments
ARG VERSION=""

# Get the image up to date
RUN dnf -y upgrade && \
    dnf clean all

# Add onedata repository
RUN dnf install -y curl && \
    curl http://onedata-dev-packages.cloud.plgrid.pl/yum/onedata_fedora_23.repo > \
    /etc/yum.repos.d/onedata.repo

# Install required packages
RUN dnf -y --enablerepo=onedata install globalregistry-${VERSION}

# Add missing CA bundle (required for server cert validation)
RUN curl -o /etc/ssl/cert.pem https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt

EXPOSE 53 443 8443 80 9443

# Add entrypoint script
ADD run.sh /root/run.sh

CMD /root/run.sh
