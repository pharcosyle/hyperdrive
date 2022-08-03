const workboxBuild = require('workbox-build');

const cachePrefix = 'hyperworker-'; // This is probably overkill.

const buildSW = (publicOutDir) => {
    return workboxBuild.generateSW({
	globDirectory: publicOutDir,
	globPatterns: [
	    '**/*'
	],
	globIgnores: [
	    'js/cljs-runtime/**/*' // Only present in development.
	],
	swDest: publicOutDir + '/service-worker.js',
	maximumFileSizeToCacheInBytes: 40 * 1024 * 1024, // Increase the limit to an arbitrary 40mb for app.js. It would be better if I just had a way to include app.js without removing the protective 2mb default. // maybe TODO
	cleanupOutdatedCaches: true,
	// I don't think these two are necessary. Reference: https://developers.google.com/web/fundamentals/primers/service-workers/lifecycle
	// skipWaiting: true,
	// clientsClaim: true,
	navigateFallback: "/index.html",
	// Workbox examples used custom cache names for this stuff so I am too even though it seems like workbox can update cache files without invalidating the entire cache.
	runtimeCaching: [{
	    urlPattern: /^https:\/\/fonts\.googleapis\.com/,
	    handler: 'StaleWhileRevalidate',
	    options: {
		cacheName: cachePrefix + 'google-fonts-stylesheets'
	    }
	}, {
	    urlPattern: /^https:\/\/fonts\.gstatic\.com/,
	    handler: 'CacheFirst',
	    options: {
		cacheName: cachePrefix + 'google-fonts-webfonts',
		cacheableResponse: {
		    statuses: [0, 200]
		},
		expiration: {
		    maxEntries: 30,
		    maxAgeSeconds: 60 * 60 * 24 * 365
		}
	    }
	}, {
	    // Stub image caching.
	    urlPattern: /\.(?:png|jpg|jpeg|svg)$/, // Maybe narrow this to clounary thumbnails specifically.
	    handler: 'StaleWhileRevalidate', // CacheFirst might be okay.
	    options: {
		cacheName: cachePrefix + 'images',
		expiration: {
		    maxEntries: 10
		}
	    }
	}]
    }).then(({warnings}) => {
	warnings.forEach(console.log);
    });
};

buildSW(process.argv[2]);
