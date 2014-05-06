module.controller('RegisterPanel', [
	'$scope',
	'authService',
	'$http',
	'$window',
	'PartyAuthorize',
	'Scraper',
	'pageService',
	function ($scope, auth, $http, $window, PartyAuthorize, Scraper, page) {
		$scope.auth = $scope.auth || auth;

		var mess = [];
		$scope.$on('$destroy', function () {
			angular.forEach(mess, function (message) {
				page.messages.remove(message);
			})
		});


		// TODO: Remove analytics
		$scope.$watch(function () {
			if (!$scope.wizard)
				return;

			for (var i = 0, l = $scope.wizard.steps.length; i < l; i++) {
				if ($scope.wizard.steps[i].active)
					return $scope.wizard.steps[i]
			}
		}, function (step) {
			if (!step)
				return;

			page.analytics.add('change', {
				type: 'wizard',
				id: step.id,
				name: step.name
			});

			page.models.Analytic.send();
		});

		$scope.$watch(function () {
			return $scope.agreement && $scope.agreement.current;
		}, function (current) {
			if (!current)
				return;

			page.analytics.add('change', {
				type: 'agreement',
				page: current
			});

			page.models.Analytic.send();
		});

		//

		$scope.wizard = {};

		var user = {
			anon: true,
			password: false,
			pending: false,
			auth: false
		};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
		};

		$scope.updateWizard = function (activate) {
			activate = angular.isUndefined(activate) ? true : activate;

			if ($scope.wizard.newStep) {
				$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
			}

			user.anon = !auth.isLoggedIn();
			user.password = auth.isPasswordPending();
			user.auth = auth.isAuthorized();

			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step, activate);
			});
		};

		//

		$scope.registerForm = {};
		$scope.userPasswordForm = {};
		$scope.authSearchForm = {};
		$scope.authApplyForm = {};
		$scope.infoForm = {};

		//

		$scope.agreement = {
			current: 1,
			total: 1,
			pages: []
		};

		Scraper('//databrary.org/policies/investigator-agreement.html')
			.then(function (pages) {
				pages = $('<response>' + pages + '</response>');
				pages.find('a').each(function () {
					$(this).attr('target', '_blank');
				});
				pages = pages.html();

				pages = pages.split(/<!--page_.*-->/).slice(1, -1);

				$scope.agreement.total = pages.length;
				$scope.agreement.pages = pages;
			});

		//

		var updateUserAuth = function () {
			PartyAuthorize.query({
				id: $scope.auth.user.id
			}, function (data) {
				angular.forEach(data.parents, function (parent) {
					user.pending = true;
				});

				$scope.updateWizard();
			}, function (res) {
				page.messages.addError({
					body: page.constants.message('register.authquery.error'),
					errors: res[0],
					status: res[1]
				})
			});
		};

		$scope.$watch('auth.user', function () {
			$scope.updateWizard();

			if (auth.isLoggedIn())
				updateUserAuth();
		});

		//

		$scope.prepareStep = {
			'register_create': function (step) {
				$scope.registerForm = step.registerForm;
				$scope.registerForm.data = {};
				$scope.registerForm.sent = false;

				$scope.registerForm.ready = function (form) {
					return $scope.registerForm.$dirty &&
						$scope.registerForm.$valid &&
						$scope.registerForm.data.name &&
						$scope.registerForm.data.email;
				};

				$scope.registerForm.proceed = function () {
					$scope.updateWizard();
				};

				//

				mess.push(page.messages.add({
					type: 'blue',
					target: '#field_name',
					body: page.constants.message('wizard.register_form.name.help')
				}));

				mess.push(page.messages.add({
					type: 'blue',
					target: '#field_email',
					body: page.constants.message('wizard.register_form.email.help')
				}));

				mess.push(page.messages.add({
					type: 'blue',
					target: '#field_affiliation',
					body: page.constants.message('wizard.register_form.affiliation.help')
				}));

				//

				var emailError = page.messages.add({
					type: 'red',
					enabled: false,
					body: page.constants.message('wizard.register_form.email.error')
				});

				mess.push(emailError);

				step.$watch('registerForm.fieldEmail.$valid', function () {
					if ($scope.registerForm.fieldEmail.$valid || !$scope.registerForm.fieldEmail.$dirty)
						page.messages.disable(emailError);
					else
						page.messages.enable(emailError);
				});
			},

			'register_agreement': function (step) {
				step.agreement = $scope.agreement;
				step.scrolled = false;
				step.$el = undefined;

				step.back = function () {
					$scope.agreement.current--;
					$scope.registerForm.data.agreement = false;
				};

				step.scroll = function ($scroll) {
					step.$el = step.$el || $scroll.$element;

					if (step.$el.height() + step.$el.scrollTop() >= step.$el.prop('scrollHeight')) {
						step.scrolled = true;
					}
				};

				step.ready = function () {
					return step.scrolled;
				};

				step.next = function () {
					$scope.agreement.current++;

					if (step.$el)
						step.$el.scrollTop(0);

					step.scrolled = false;
				};

				step.proceed = function () {
					$scope.registerForm.data.agreement = true;
					step.scrolled = false;

					$http
						.post('/register', $scope.registerForm.data)
						.success(function (res) {
							$scope.registerForm.sent = true;
							$scope.updateWizard();
						})
						.error(function (errors, status) {
							page.messages.addError({
								body: page.constants.message('error.generic'),
								errors: errors,
								status: status
							});
						});
				};
			},

			'register_email': function (step) {
				step.registerForm = $scope.registerForm;
			},

			'register_password': function (step) {
				$scope.userPasswordForm = step.userPasswordForm;
				$scope.userPasswordForm.sent = false;

				$scope.userPasswordForm.saveSuccessFn = function (form) {
					$scope.userPasswordForm.sent = true;
					$scope.updateWizard();
				};
			},

			'register_agent': function (step) {
				$scope.authSearchForm = step.authSearchForm;
				$scope.authSearchForm.data = {};

				$scope.authSearchForm.selectFn = function (found) {
					$scope.authSearchForm.data.party = found;
					$scope.infoForm.data.query = undefined;
					$scope.updateWizard();
				};

				$scope.authSearchForm.notFoundFn = function (query) {
					$scope.authSearchForm.data.party = true;
					$scope.infoForm.data.query = query;
					$scope.updateWizard();
				};
			},

			'register_request': function (step) {
				$scope.authApplyForm = step.authApplyForm;
				$scope.authApplyForm.sent = false;

				$scope.infoForm = step.infoForm;
				$scope.infoForm.data = {};

				step.ifInfo = function () {
					return angular.isString($scope.infoForm.data.query);
				};

				//

				$scope.authApplyForm.successFn = function (form) {
					$scope.authApplyForm.sent = true;
					updateUserAuth();
					$scope.updateWizard();
				};

				$scope.authApplyForm.cancelFn = function (form) {
					$scope.authSearchForm.data.party = undefined;
					$scope.authApplyForm.other = undefined;
					$scope.updateWizard();
				};

				//

				$scope.infoForm.ready = function () {
					return $scope.infoForm.$dirty &&
						$scope.infoForm.$valid &&
						$scope.infoForm.data.info;
				};

				$scope.infoForm.proceed = function () {
					$http
						.get('/api/party/' + auth.user.id + '/authorize/search', {
							params: {
								apply: true,
								notfound: true,
								name: $scope.infoForm.data.query,
								info: $scope.infoForm.data.info
							}
						})
						.success(function (data) {
							$scope.authApplyForm.successFn();
						})
						.error(function (errors, status) {
							page.messages.addError({
								body: page.constants.message('error.generic'),
								errors: errors,
								status: status
							});
						});
				};

				$scope.infoForm.cancel = function () {
					$scope.infoForm.data.info = '';
					$scope.authSearchForm.data.party = undefined;
					$scope.authApplyForm.other = undefined;
					$scope.infoForm.data.query = undefined;
					$scope.updateWizard();
				};
			},

			'register_pending': function (step) {
			}
		};

		//

		$scope.updateStep = {
			'register_create': function (step, activate) {
				step.allow = user.anon && !user.password && !$scope.registerForm.sent;

				if (activate && user.anon && !user.password && !$scope.registerForm.ready())
					$scope.wizard.activateStep(step);

				step.complete = !user.anon || user.password || !!$scope.registerForm.ready();
			},

			'register_agreement': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent)
					$scope.wizard.activateStep(step);

				step.complete = !user.anon || user.password || !!$scope.registerForm.sent;
			},

			'register_email': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.sent)
					$scope.wizard.activateStep(step);

				step.complete = !user.anon || user.password;
			},

			'register_password': function (step, activate) {
				step.allow = user.anon && user.password && !$scope.userPasswordForm.sent;

				if (activate && user.anon && user.password && !$scope.userPasswordForm.sent)
					$scope.wizard.activateStep(step);

				step.complete = !user.anon;
			},

			'register_agent': function (step, activate) {
				step.allow = !user.anon && !user.pending && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && !$scope.authSearchForm.data.party)
					$scope.wizard.activateStep(step);

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authSearchForm.data.party);
			},

			'register_request': function (step, activate) {
				step.allow = !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent)
					$scope.wizard.activateStep(step);

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authApplyForm.sent);

				//

				$scope.authApplyForm.party = $scope.auth.user;

				var perm = [];

				if ($scope.authSearchForm.institution) {
					perm = [2, 0];
					step.institution = true;
				} else {
					perm = [2, 2];
					step.institution = false;
				}

				if ($scope.authSearchForm.data.party && !$scope.authApplyForm.other)
					$scope.authApplyForm.other = {
						id: $scope.authSearchForm.data.party.id,
						party: $scope.authSearchForm.data.party,
						inherit: perm[2],
						direct: perm[1]
					};
			},

			'register_pending': function (step, activate) {
				step.allow = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				if (activate)
					step.active = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				step.complete = user.auth;
			}
		};
	}
]);
