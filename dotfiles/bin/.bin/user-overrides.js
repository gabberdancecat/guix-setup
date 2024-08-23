//// ##################################################### ////

//I did not read the wiki
//Mostly copied from Trafotin:
//https://gitlab.com/trafotin/dotfiles/-/blob/main/user-overrides.js
//Wiki overrides:
//https://github.com/arkenfox/user.js/wiki/3.2-Overrides-%5BCommon%5D

///// ------------------- On Shutdown: ------------------- ////

//Delete cookies on close, but see below to make exceptions
/* 2801: delete cookies and site data on exit
//* 0=keep until they expire (default), 2=keep until you close Firefox
* [NOTE] A "cookie" block permission also controls localStorage/sessionStorage, indexedDB,
* sharedWorkers and serviceWorkers. serviceWorkers require an "Allow" permission
* [SETTING] Privacy & Security>Cookies and Site Data>Delete cookies and site data when Firefox is closed
* [SETTING] to add site exceptions: Ctrl+I>Permissions>Cookies>Allow
* [SETTING] to manage site exceptions: Options>Privacy & Security>Permissions>Settings ***/
// user_pref("network.cookie.lifetimePolicy", 2);
user_pref("network.cookie.lifetimePolicy", 0); // dont delete cookies
//Disabling disk cache is better, but try this if you like performance
// user_pref("privacy.clearsitedata.cache.enabled", true);

user_pref("privacy.sanitize.sanitizeOnShutdown", true);

/// ### Clear on Shutdown: ### ///

user_pref("privacy.clearOnShutdown.cache", true);   // [DEFAULT: true]
user_pref("privacy.clearOnShutdown_v2.cache", true);  // [FF128+] [DEFAULT: true]
user_pref("privacy.clearOnShutdown.downloads", true); // [DEFAULT: true]
user_pref("privacy.clearOnShutdown.formdata", true);  // [DEFAULT: true]
user_pref("privacy.clearOnShutdown.history", false);   // [DEFAULT: true]
user_pref("privacy.clearOnShutdown_v2.historyFormDataAndDownloads", false); // [FF128+] [DEFAULT: true]
// user_pref("privacy.clearOnShutdown.siteSettings", false); // [DEFAULT: false]
// user_pref("privacy.clearOnShutdown_v2.siteSettings", false); // [FF128+] [DEFAULT: false]

user_pref("privacy.clearOnShutdown.cookies", false); // Cookies [DEFAULT: true]
user_pref("privacy.clearOnShutdown.offlineApps", false); // Site Data [DEFAULT: true]
user_pref("privacy.clearOnShutdown.sessions", false);  // Active Logins [DEFAULT: true]
user_pref("privacy.clearOnShutdown_v2.cookiesAndStorage", false); // Cookies, Site Data, Active Logins [FF128+]


/// ### Manual wiping, such as the forget-me-not button (C-S-<del>): ### ///

user_pref("privacy.cpd.cache", true);    // [DEFAULT: true]
user_pref("privacy.clearHistory.cache", true);    // [DEFAULT: true]
user_pref("privacy.cpd.formdata", true); // [DEFAULT: true]
user_pref("privacy.cpd.history", false);  // [DEFAULT: true]
// user_pref("privacy.cpd.downloads", true); // not used, see note above
user_pref("privacy.clearHistory.historyFormDataAndDownloads", true);
user_pref("privacy.cpd.cookies", false); 
user_pref("privacy.cpd.sessions", false); // [DEFAULT: true]
user_pref("privacy.cpd.offlineApps", false); // [DEFAULT: false]
user_pref("privacy.clearHistory.cookiesAndStorage", false);

/// ?
// user_pref("privacy.cpd.siteSettings", false); // [DEFAULT: false]
// Delete everything ever.
user_pref("privacy.clearHistory.siteSettings", false);

//Helps against forensic tools.
user_pref("places.history.enabled", true); // [DEFAULT: false]


///// --------------------- Privacy: --------------------- ////

/// ### Site privacy: ### ///

// Strict third party requests, may cause images/video to break.
// user_pref("network.http.referer.XOriginPolicy", 2); // too strict
user_pref("network.http.referer.XOriginPolicy", 0); // librewolf default (DEFAULT)

user_pref("privacy.resistFingerprinting", true); // RPF
user_pref("privacy.resistFingerprinting.letterboxing", true); // optional

/// ???
user_pref("privacy.spoof_english", 2); // optional

/// ### Network: ### ///

// DNS over HTTPS
// Disable if you use PiHole, but tools like pfBlocker work fine.
// Cloudflare: https://mozilla.cloudflare-dns.com/dns-query

/* 0712: set DoH provider
 * The custom uri is the value shown when you "Choose provider>Custom>"
 * [NOTE] If you USE custom then "network.trr.uri" should be set the same
 * [SETTING] Privacy & Security>DNS over HTTPS>Increased/Max>Choose provider ***/
   // user_pref("network.trr.uri", "https://example.dns");
   // user_pref("network.trr.custom_uri", "https://example.dns");
user_pref("network.trr.uri", "https://doh.mullvad.net/dns-query");
/* 0710: enable DNS-over-HTTPS (DoH) [FF60+]
 * 0=default, 2=increased (TRR (Trusted Recursive Resolver) first), 3=max (TRR only), 5=off (no rollout)
 * see "doh-rollout.home-region": USA 2019, Canada 2021, Russia/Ukraine 2022 [3]
 * [SETTING] Privacy & Security>DNS over HTTPS
 * [1] https://hacks.mozilla.org/2018/05/a-cartoon-intro-to-dns-over-https/
 * [2] https://wiki.mozilla.org/Security/DOH-resolver-policy
 * [3] https://support.mozilla.org/en-US/kb/firefox-dns-over-https
 * [4] https://www.eff.org/deeplinks/2020/12/dns-doh-and-odoh-oh-my-year-review-2020 ***/
user_pref("network.trr.mode", 2); // 

/* 5509: disable IPv6 if using a VPN
 * This is an application level fallback. Disabling IPv6 is best done at an OS/network
 * level, and/or configured properly in system wide VPN setups.
 * [SETUP-WEB] PR_CONNECT_RESET_ERROR
 * [NOTE] PHP defaults to IPv6 with "localhost". Use "php -S 127.0.0.1:PORT"
 * [TEST] https://ipleak.org/
 * [1] https://www.internetsociety.org/tag/ipv6-security/ (Myths 2,4,5,6) ***/
user_pref("network.dns.disableIPv6", false);

/// ### Sane privacy defaults: ### ///

//WebGL is a security risk, but sometimes breaks things like 23andMe
//or Google Maps (not always).
user_pref("webgl.disabled", true);
//Always send a "Do Not Track" signal
user_pref("privacy.donottrackheader.enabled", true); // [DEFAULT: false]

/// ### Sane security defaults: ### ///

//Firefox stores passwords in plain text and obsolete if you use a password manager.
//Mozilla also told people to stop using their password manager.
user_pref("signon.rememberSignons", false);
//Disable Pocket, it's proprietary trash
user_pref("extensions.pocket.enabled", false);
//Disable Mozilla account
user_pref("identity.fxaccounts.enabled", false);


///// ------------------ Functionality: ------------------ ////

/// ### Performance: ### ///

//Disk caching, which might improve performance if enabled.
//user_pref("browser.cache.disk.enable", true); // [DEFAULT: false]

/// ### Features: ### ///

//WebRTC settings, things like video calls
// user_pref("media.peerconnection.enabled", false);
// Disable Media Plugins
// user_pref("media.gmp-provider.enabled", false);
// Disable DRM, FCKDRM
// user_pref("media.gmp-widevinecdm.enabled", false);
user_pref("media.eme.enabled", false);

/// ### Adblock: ### ///

//Use Disconnect's blocklist to block ads
user_pref("browser.contentblocking.category", "strict");


///// ----------------- Personalization: ----------------- ////

/// ### Searching: ### ///

//Default search engine (doesn't work)
user_pref("browser.urlbar.placeholderName", "DuckDuckGo"); // Set DuckDuckGo as a search provider
//Reenable search engines
user_pref("keyword.enabled", true);
//Enable Search Engine suggestion
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.urlbar.suggest.searches", false);

/// ### Sane defaults (UI): ### ///

// : restore pages? (unsure)
user_pref("browser.startup.page", 3); // 0102
//Enable favicons, the icons in bookmarks
user_pref("browser.shell.shortcutFavicons", true);
//Enable Mozilla Container Tabs
//Redundant with Total Cookie Protection, but useful if you have multiple accounts
//with the same provider (e.g. a work Google account and a personal Google account)
user_pref("privacy.userContext.enabled", true);
user_pref("privacy.userContext.ui.enabled", true);
//Autoplaying settings
//0=Allow all, 1=Block non-muted media (default), 5=Block all
// user_pref("media.autoplay.default", 5);
//If some websites REALLY need autoplaying...
//0=sticky (default), 1=transient, 2=user
// user_pref("media.autoplay.blocking_policy", 2);
user_pref("media.autoplay.blocking_policy", 0); // librewolf default

/// ### Theming: ### ///

// Dark theme (prolly doesn't work)
user_pref("extensions.activeThemeID", "firefox-compact-dark@mozilla.org");
user_pref("browser.theme.content-theme", 0); // dark on librewolf
user_pref("browser.theme.toolbar-theme", 0);

