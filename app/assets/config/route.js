module.config([
	'$routeProvider',
	function ($routeProvider) {
		$routeProvider.when('/', {
			controller: 'HomeView',
			templateUrl: 'homeView.html',
			resolve: {
				parties: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Party.query({
							access: page.permission.CONTRIBUTE
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						return deferred.promise;
					}
				],
				volume: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						if (page.auth.isAuthorized()) {
							page.models.Volume.get({
								id: 8,
								access: ''
							}, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						} else {
							deferred.resolve({});
						}

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: false
		});

		//

		$routeProvider.when('/login', {
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/register', {
			controller: 'RegisterView',
			templateUrl: 'registerView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/password', {
			controller: 'ResetView',
			templateUrl: 'resetView.html',
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/asset/formats', {
			controller: 'HelpFormatsView',
			templateUrl: 'helpFormatsView.html',
			reloadOnSearch: false,
			authenticate: true,
		});

		//

		$routeProvider.when('/token/:id', {
			controller: (function () {
				return window.$play.object && window.$play.object.reset ? 'ResetView' : 'RegisterView';
			}()),
			templateUrl: function () {
				return window.$play.object && window.$play.object.reset ? 'resetView.html' : 'registerView.html';
			},
			resolve: {
				token: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						if (page.$window.$play.object && page.$window.$play.object.auth) {
							deferred.resolve(page.$window.$play.object);
						} else {
							page.$http
								.get('/api/token/' + page.$route.current.params.id + '?auth=' + page.$route.current.params.auth)
								.success(function (res) {
									page.$window.$play.object = res;
									deferred.resolve(res);
								})
								.error(function (res) {
									deferred.reject(res);
									page.$location.url('/');
								});
						}

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false
		});

		//

		$routeProvider.when('/search', {
			controller: 'SearchView',
			templateUrl: 'searchView.html',
			resolve: {
				volumes: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Volume.query({}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		});

		//

		var partyView = {
			controller: 'PartyView',
			templateUrl: 'partyView.html',
			resolve: {
				party: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						var req = {
							comments: '',
							access: '',
							openid: '',
							duns: '',
							parents: '',
							children: '',
						};

						if (page.$route.current.params.id) {
							req.id = page.$route.current.params.id;
						} else if (page.auth.isLoggedIn()) {
							req.id = page.auth.user.id;
						} else if (page.types.isParty(page.$window.$play.object)) {
							req.id = page.$window.$play.object.id;
						}

						if (page.$route.current.params.id) {
							page.models.Party.get(req, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						} else {
							page.models.Party.profile(req, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						}

						return deferred.promise;
					}
				],
				volumes: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						var req = {
							id: null
						};

						if (page.$route.current.params.id) {
							req.party = page.$route.current.params.id;
						} else if (page.auth.isLoggedIn()) {
							req.party = page.auth.user.id;
						} else if (page.types.isParty(page.$window.$play.object)) {
							req.party = page.$window.$play.object.id;
						}

						if (page.auth.isUnauthorized()) {
							return [];
						}

						page.models.Volume.query(req, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		};

		$routeProvider.when('/party/:id', partyView);
		$routeProvider.when('/profile', partyView);

		//

		var partyEditParty;

		$routeProvider.when('/party/:id/edit', {
			controller: 'PartyEditView',
			templateUrl: 'partyEditView.html',
			resolve: {
				party: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Party.get({
							id: page.$route.current.params.id,
							duns: '',
							parents: '',
							children: '',
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						partyEditParty = deferred.promise;
						return deferred.promise;
					}
				],
				partyAuth: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						var empty = {
							parents: {},
							children: {},
						};

						partyEditParty.then(function (party) {
							if (page.auth.hasAccess('ADMIN', party)) {
								page.models.PartyAuthorize.query(function (res) {
									deferred.resolve(res);
								}, function (res) {
									deferred.reject(res);
								});
							} else {
								deferred.resolve(empty);
							}
						}, function () {
							deferred.resolve(empty);
						});

						return deferred.promise;
					}
				],
			},
			reloadOnSearch: false,
			authenticate: true
		});

		//

		var volumeEditVolume;

		var volumeEdit = {
			controller: 'VolumeEditView',
			templateUrl: 'volumeEditView.html',
			resolve: {
				volume: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						if (!page.$route.current.params.id) {
							deferred.resolve();
						} else {
							page.models.Volume.get({
								access: '',
								citation: '',
								top: '',
								funding: '',
							}, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						}

						volumeEditVolume = deferred.promise;
						return volumeEditVolume;
					}
				],
				slot: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						if (!page.$route.current.params.id) {
							deferred.resolve();
						} else {
							volumeEditVolume.then(function (volume) {
								page.models.Slot.get({
									id: volume.top.id,
									vid: volume.id,
									assets: '',
								}, function (res) {
									deferred.resolve(res);
								}, function (res) {
									deferred.reject(res);
								});
							});
						}

						return deferred.promise;
					}
				],
			},
			reloadOnSearch: false,
			authenticate: true
		};

		$routeProvider.when('/volume/create', volumeEdit);
		$routeProvider.when('/volume/:id/edit', volumeEdit);

		//

		$routeProvider.when('/volume/:id', {
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			resolve: {
				volume: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Volume.get({
							access: '',
							citation: '',
							funding: '',
							providers: '',
							consumers: '',
							top: '',
							tags: '',
							excerpts: '',
							comments: '',
							records: '',
							summary: '',
							sessions: '',
							categories: ''
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		});

		//

		$routeProvider.when('/volume/:vid/slot/:id', {
			controller: 'SlotView',
			controllerAs: 'view',
			templateUrl: 'slotView.html',
			resolve: {
				slot: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Slot.get({
							assets: '',
							records: '',
							tags: '',
							comments: '',
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.reject(res);
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: true
		});

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}
]);

module.run([
	'pageService', function (page) {
		page.$rootScope.$on('$routeChangeStart', function (event, next) {
			if (page.auth.isLoggedIn()) {
				if (page.auth.isUnauthorized()) {
					if (!next.$$route) {
						page.$location.url(page.router.register());
					}

					var controller = angular.isFunction(next.$$route.controller) ? next.$$route.controller() : next.$$route.controller;

					if (controller !== 'RegisterView') {
						page.$location.url(page.router.register());
					}
				} else if (!next.authenticate && next.$$route.controller !== 'ErrorView' && next.$$route.originalPath !== '/profile') {
					page.$location.url(page.router.index());
				}
			} else {
				if (page.auth.isPasswordPending() && next.$$route && next.$$route.controller != 'RegisterView' && (!angular.isFunction(next.$$route.controller) || next.$$route.controller() != 'RegisterView')) {
					page.$location.url(page.router.register());
				} else if (next.authenticate) {
					page.$location.url(page.router.index());
				}
			}
		});
	}
]);


