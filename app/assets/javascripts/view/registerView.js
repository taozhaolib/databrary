module.controller('RegisterView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.register');

		//

		$scope.auth = $scope.auth || page.auth;

		// TODO: Remove analytics
		$scope.$watch(function () {
			if (!$scope.wizard) {
				return;
			}

			for (var i = 0, l = $scope.wizard.steps.length; i < l; i++) {
				if ($scope.wizard.steps[i].active) {
					return $scope.wizard.steps[i]
				}
			}
		}, function (step) {
			if (!step) {
				return;
			}

			page.analytics.add('change', {
				type: 'wizard',
				id: step.id,
				name: step.name
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

			user.anon = !page.auth.isLoggedIn();
			user.password = page.auth.isPasswordPending();
			user.auth = page.auth.isAuthorized();

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

		var updateUserAuth = function () {
			page.models.PartyAuthorize.query({
				id: page.auth.user.id
			}, function (data) {
				angular.forEach(data.parents, function (parent) {
					user.pending = true;
				});

				$scope.updateWizard();
			}, function (res) {
				page.messages.addError({
					body: page.constants.message('register.authquery.error'),
					report: res,
				})
			});
		};

		$scope.$watch('auth.user', function () {
			$scope.updateWizard();

			if (page.auth.isLoggedIn()) {
				updateUserAuth();
			}
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

				$scope.registerForm.validator.client({
					fieldName: {
						tips: page.constants.message('wizard.register_form.name.help'),
					},
					fieldEmail: {
						errors: page.constants.message('wizard.register_form.email.error'),
						tips: page.constants.message('wizard.register_form.email.help'),
					},
					fieldAffiliation: {
						tips: page.constants.message('wizard.register_form.affiliation.help'),
					},
				}, true);
			},

			'register_agreement': function (step) {
				step.ready = function () {
					return step.agreementCheckbox;
				};

				step.proceed = function () {
					$scope.registerForm.data.agreement = true;

					page.$http
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
					page.$http
						.get('/api/party/' + page.auth.user.id + '/authorize/search', {
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
							$scope.infoForm.messages.addError({
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

				if (activate && user.anon && !user.password && !$scope.registerForm.ready()) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password || !!$scope.registerForm.ready();
			},

			'register_agreement': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password || !!$scope.registerForm.sent;
			},

			'register_email': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password;
			},

			'register_password': function (step, activate) {
				step.allow = user.anon && user.password && !$scope.userPasswordForm.sent;

				if (activate && user.anon && user.password && !$scope.userPasswordForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon;
			},

			'register_agent': function (step, activate) {
				step.allow = !user.anon && !user.pending && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && !$scope.authSearchForm.data.party) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authSearchForm.data.party);
			},

			'register_request': function (step, activate) {
				step.allow = !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authApplyForm.sent);

				//

				$scope.authApplyForm.party = page.auth.user;

				var perm = [];

				if ($scope.authSearchForm.institution) {
					perm = [2, 0];
					step.institution = true;
				} else {
					perm = [2, 2];
					step.institution = false;
				}

				if ($scope.authSearchForm.data.party && !$scope.authApplyForm.other) {
					$scope.authApplyForm.other = {
						id: $scope.authSearchForm.data.party.id,
						party: $scope.authSearchForm.data.party,
						inherit: perm[2],
						direct: perm[1]
					};
				}
			},

			'register_pending': function (step, activate) {
				step.allow = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				if (activate && !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent)) {
					$scope.wizard.activateStep(step);
				}

				step.complete = user.auth;
			}
		};
	}
]);
