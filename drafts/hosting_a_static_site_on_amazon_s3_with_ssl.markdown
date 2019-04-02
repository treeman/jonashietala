---
title: "Hosting a static site on Amazon S3 with SSL"
tags: Gaming, Ludum Dare
---

# Host a static site on S3

Bucket

Now you can visit the site, for example: <http://whycryptocurrencies.com.s3-website.eu-north-1.amazonaws.com/>


# Custom domain

CNAME Record   @   whycryptocurrencies.com.s3-website.eu-north-1.amazonaws.com.  TTL: Automatic

You can add a redirect from www. to @ (or vice versa) if you want.

You can now visit your domain, for example: <http://whycryptocurrencies.com>, but without SSL.


# CloudFront

Web distribution.

1. Origin domain name
2. Redirect HTTP to HTTPS
3. Compress objects automatically
4. Alternative domain names (whycryptocurrencies.com)
5. Use cloudfront SSL certificate for now, we'll come back and change this later
6. Default root object: index.html

When it's deployed we can test access by visiting the url under Domain Name, for example <d29jm6e61zesqa.cloudfront.net>. You will get a "Your connection is not secure" message, which is expected as cloudflare's certificate isn't valid for the bucket address.

We now need to change the CNAME record for the domain to the cloudfront endpoint.

CNAME Record   @   d29jm6e61zesqa.cloudfront.net.  TTL: Automatic

Now when we visit our domain we'll get the same SSL warning.


# Enable SSL via AWS Certificate Manager

Request a certificate.
Add both "whycryptocurrencies.com" and "\*.whycryptocurrencies.com" as domain names.
You can use either DNS or email validation.

If you use DNS validation you'll be asked to add a new subdomain with a CNAME value. Like so:

CNAME Record 	\_c20fab3bbd32430576cfdcdd43b090d1 \_7e714a15b4afabada0784649c5eac502.hkvuiqjoua.acm-validations.aws.  Automatic 

Then we need to tell CloudFront to use the new certificate. Go back and edit the General settings and choose the Custom SSL Certificate option.

