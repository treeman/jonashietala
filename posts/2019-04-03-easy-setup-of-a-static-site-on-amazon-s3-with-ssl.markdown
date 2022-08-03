---
title: "Easy setup of a static site on Amazon S3 with SSL"
tags: Webpage
---

I've been hosting my site on Amazon S3 for a while now but I never activated SSL for it. I just never got around to it, probably the usual procrastination.

When I had to setup a new site for another project I chose to host on Amazon as well. Although there are many other free options, why switch when it's working? This time I activated SSL as well---and documented it. It's all quite easy.

# Host a static site on S3

First we need to [create a bucket on S3][S3], it's straightforward. When you're done activate static website hosting:

[S3]: https://console.aws.amazon.com/s3/home

![Make sure to add the Index document.](/images/s3_setup/static_website.png)

I use a [crappy script][] to sync my site, but there are many other tools. You could also upload files via the [S3 console][S3] to get started. When you've uploaded a site you can visit the Endpoint to confirm that it works. For example:

<http://whycryptocurrencies.com.s3-website.eu-north-1.amazonaws.com/>

[crappy script]: https://github.com/treeman/jonashietala/blob/master/sync


# Custom domain

This is a really crappy url, so let's use our custom domain. Add a CNAME record pointing to the Endpoint. It might be a good idea to add a redirect from www to @ (or vice versa). This is how it might look on [namecheap][]:

![Here I redirect from www to @, but it really doesn't matter which way.](/images/s3_setup/s3_domains.png)

After waiting for the settings to update, which can be annoying for someone as impatient as myself, you can then visit the site from your domain.

For example <http://whycryptocurrencies.com>, but without SSL support.

[namecheap]: https://www.namecheap.com/


# CloudFront

To enable SSL we need a CloudFront distribution. Don't worry, it's also free and might come with other benefits.

Start by [creating a distribution][cloudfront]. Make sure to create a web distribution and go through these options:

1. Origin domain name
   Select your S3 bucket
2. Redirect HTTP to HTTPS
3. Compress objects automatically
4. Alternative domain names, input your domain. Like `whycryptocurrencies.com`
5. Use the default cloudfront SSL certificate for now, we'll change this later
6. Default root object: `index.html`

When it's deployed, wait for it, we can test access by visiting the address under Domain Name. For me that was <http://d29jm6e61zesqa.cloudfront.net>.

You will get redirected to a https address but we need to update our custom domain to point to the cloudfront address. Like so:

![](/images/s3_setup/namecheap_cloudfront.png)

When the settings have updated and we visit our domain we will get a "Insecure Connection" warning because the SSL certificate we're using is only valid for cloudfront endpoints, not our own domain. Fixing this is the final step.

[cloudfront]: https://console.aws.amazon.com/cloudfront/home


# Enable SSL via AWS Certificate Manager

At first I thought I had to use [let's encrypt][letssencrypt] to create a SSL certificate. They are great but all guides I could see involved a bunch of command line work, which you had to do regularly to renew your certificate. But there's an easier way directly in AWS, which is also free.

If we go back to our CloudFront distribution end Edit the General settings we'll see the SSL Certificate settings:


Click on the "Request or Import a Certificate with ACM" to get to the [AWS Certificate Manager][] (or just lick the link).

Make sure to input both your domain and a subaddress wildcard as domain names. I used `whycryptocurrencies.com` and `*.whycryptocurrencies.com` for example.

It doesn't matter if you use DNS or email validation.  If you choose DNS validation you'll be asked to add a new subdomain with a CNAME value. Like so:

![\_c20fab3bbd32430576cfdcdd43b090d1 is the subdomain with the value \_7e714a15b4afabada0784649c5eac502.hkvuiqjoua.acm-validations.aws.](/images/s3_setup/DNS_validation.png)

... And we'll need to wait again until it's validated.

Finally we just need to tell CloudFront to use the new certificate. Go back and edit the General settings choose the Custom SSL Certificate option:

![](/images/s3_setup/cert.png)

And we should be done! Now <https://whycryptocurrencies.com> should give a valid cert and http should redirect to https.

Remember how I said we need patience? When I did all these steps firefox kept warning me about a bad SSL cert and I couldn't for the life of me see where I went wrong. After a while I gave up and went to bed, but the next day it all magically worked. Turns out I didn't have enough patience.

You know some say sleeping solves your problems? In this case it literally did.


[letssencrypt]: https://letsencrypt.org/
[AWS Certificate Manager]: https://console.aws.amazon.com/acm/home

