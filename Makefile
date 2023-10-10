REGISTRY_HOST=docker.io
USERNAME=applibs
NAME=$(shell basename "$(CURDIR)")
IMAGE=$(REGISTRY_HOST)/$(USERNAME)/$(NAME)

.PHONY: build test shell release clean

build:
	docker build --platform=linux/amd64 -t $(IMAGE) .

test:
	docker run --rm -v `pwd`:`pwd` -w /app  -e ISO_FILENAME=`pwd`/isochrones_no_overlap.rds -e CENTERS_FILENAME=`pwd`/ctsa_centers.csv $(IMAGE) `pwd`/out.csv `pwd`/out2.csv
	# docker run --rm -v `pwd`:`pwd` -w /app  -e ISO_FILENAME=`pwd`/isochrones-10hr_15min+60min_no_overlap.rds -e CENTERS_FILENAME=`pwd`/ctsa_centers.csv $(IMAGE) `pwd`/out.csv `pwd`/out2.csv
	# docker run --rm -v `pwd`:`pwd` -w /app  -e ISO_FILENAME=`pwd`/isochrones.rds -e CENTERS_FILENAME=`pwd`/ctsa_centers.csv $(IMAGE) `pwd`/out.csv `pwd`/out2.csv
	# docker run --rm -v `pwd`:`pwd` -w /app  -e ISO_FILENAME=`pwd`/isochrones.rds -e CENTERS_FILENAME=`pwd`/ctsa_centers.csv $(IMAGE) `pwd`/test/my_address_file.csv `pwd`/out.csv
	# docker run --rm -v "${PWD}/test":/tmp $(IMAGE) my_address_file.csv
	# docker run --rm -v "${PWD}/test":/tmp $(IMAGE) my_address_file.csv 0.6
	# docker run --rm -v "${PWD}/test":/tmp $(IMAGE) my_address_file.csv all

shell:
	docker run --rm -it --entrypoint=/bin/bash -v "${PWD}":"${PWD}" $(IMAGE)

release:
ifndef VERSION
	$(error VERSION is not set. Usage: "make release VERSION=X.X")
endif
ifndef DOCKER_USERNAME
	$(error DOCKER_USERNAME is not set)
endif
ifndef DOCKER_PAT
	$(error DOCKER_PAT is not set)
endif
	git commit -am "Release for image version $(VERSION)" --allow-empty
	git tag -a $(VERSION) -m "${VERSION}"
	git push origin ${VERSION}
	git push
	echo "${DOCKER_PAT}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
	docker tag ${IMAGE}:latest ${IMAGE}:${VERSION}
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest

clean:
	docker system prune -f
