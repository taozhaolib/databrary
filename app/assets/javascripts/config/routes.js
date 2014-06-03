module.config([
	'$locationProvider',
	'$routeProvider',
	'$httpProvider',
	function ($locationProvider, $routeProvider, $httpProvider) {
		$locationProvider.html5Mode(true);

		//

		var appResolve = function (config) {
			if (!config.resolve) {
				config.resolve = {};
			}

			angular.extend(config.resolve, {
				authPromise: [
					'authService', function (auth) {
						return auth.$promise;
					}
				],
				constantPromise: [
					'constantService', function (constants) {
						return constants.$promise;
					}
				]
			});

			return config;
		};

		//

		$routeProvider.when('/', appResolve({
			controller: 'HomeView',
			templateUrl: 'homeView.html',
			resolve: {
				parties: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.constants.$promise.then(function () {
							page.models.Party.query({
								access: page.permission.CONTRIBUTE
							}, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						});

						return deferred.promise;
					}
				],
				volume: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						if(page.auth.hasAuth())

						page.models.Volume.get({
							id: 8,
							access: ''
						}, function (res) {
							deferred.resolve(res);
						}, function (res) {
							deferred.resolve({});
						});

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false,
			authenticate: false
		}));

		//

		$routeProvider.when('/login', appResolve({
			controller: 'LoginView',
			templateUrl: 'loginView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/register', appResolve({
			controller: 'RegisterView',
			templateUrl: 'registerView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/password', appResolve({
			controller: 'ResetView',
			templateUrl: 'resetView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/error', appResolve({
			controller: 'ErrorView',
			templateUrl: 'errorView.html',
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/token/:id', appResolve({
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
								.error(function () {
									deferred.reject(res);
									page.$location.url('/');
								});
						}

						return deferred.promise;
					}
				]
			},
			reloadOnSearch: false
		}));

		//

		$routeProvider.when('/search', appResolve({
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
		}));

		//

		var partyView = appResolve({
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
							children: ''
						};

						if (page.$route.current.params.id) {
							req.id = page.$route.current.params.id;
						}
						else if (page.auth.isLoggedIn()) {
							req.id = page.auth.user.id;
						}
						else if (page.types.isParty(page.$window.$play.object)) {
							req.id = page.$window.$play.object.id;
						}

						if (page.$route.current.params.id) {
							page.models.Party.get(req, function (res) {
								deferred.resolve(res);
							}, function (res) {
								deferred.reject(res);
							});
						}
						else {
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
						}
						else if (page.auth.isLoggedIn()) {
							req.party = page.auth.user.id;
						}
						else if (page.types.isParty(page.$window.$play.object)) {
							req.party = page.$window.$play.object.id;
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
		});

		$routeProvider.when('/party/:id', partyView);
		$routeProvider.when('/profile', partyView);

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
								citations: '',
								top: '',
								excerpts: '',
								funding: ''
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
									segment: ',',
									assets: ''
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

		$routeProvider.when('/volume/create', appResolve(volumeEdit));
		$routeProvider.when('/volume/:id/edit', appResolve(volumeEdit));

		//

		$routeProvider.when('/volume/:id', appResolve({
			controller: 'VolumeView',
			templateUrl: 'volumeView.html',
			resolve: {
				volume: [
					'pageService', function (page) {
						var deferred = page.$q.defer();

						page.models.Volume.get({
							access: '',
							citations: '',
							providers: '',
							consumers: '',
							top: '',
							tags: '',
							excerpts: '',
							comments: '',
							records: '',
							summary: '',
							sessions: '',
							categories: '',
							funding: ''
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
		}));

		//

		$routeProvider.otherwise({
			redirectTo: '/search'
		});
	}
]);

module.run([
	'pageService', function (page) {
		page.$rootScope.$on('$routeChangeStart', function (event, next) {
			page.auth.$promise.then(function () {
				if (page.auth.isLoggedIn()) {
					if (page.auth.isUnauthorized()) {
						if (!next.$$route || next.$$route.controller != 'RegisterView' || (angular.isFunction(next.$$route.controller) && next.$$route.controller() != 'RegisterView')) {
							page.$location.url(page.router.register());
						}
					}
				} else {
					if (page.auth.isPasswordPending() && next.$$route && next.$$route.controller != 'RegisterView' && (!angular.isFunction(next.$$route.controller) || next.$$route.controller() != 'RegisterView')) {
						page.$location.url(page.router.register());
					} else if (next.authenticate) {
						page.$location.url(page.router.index());
					}
				}
			});
		});
	}
]);


