'use strict';

module.controller('NetworkPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.bootPanel = function () {
      getPartyAuth();
    };

    $scope.refreshPanel = function () {
      getPartyAuth();
    };

    //

    $scope.partyAuth = {
      parents: [],
      children: [],
    };

    var actionMessages = {};

    $scope.$on('$destroy', function () {
      angular.forEach(actionMessages, function (bundle) {
        page.messages.remove(bundle.message);
      });
    });

    var getPartyAuth = function () {
      if (page.auth.hasAccess(page.permission.ADMIN, $scope.party)) {
        page.models.partyAuthorize.$cache.removeAll();
        page.models.partyAuthorize.query(function (data) {
          $scope.partyAuth = data;

          angular.forEach($scope.partyAuth.children, function (party) {
            if (!party.member && !party.site) {
              if (!actionMessages[party.id]) {
                actionMessages[party.id] = {
                  party: party,
                  message: page.messages.add({
                    type: 'yellow',
                    closeable: true,
                    body: page.$compile('<span>' + page.constants.message('auth.pending.notice', party.party.name) + ' <a href="' + $scope.party.editRoute('grant') + '">Manage</a>.</span>')($scope)
                  })
                };
              }
              else {
                actionMessages[party.id].party = party;
              }
            }
          });
        }, function (res) {
          page.messages.addError({
            body: page.constants.message('network.authquery.error'),
            report: res,
          });
        });
      } else {
        page.models.Party.get(page.$routeParams.id || page.auth.user.id, {
          parents: '',
          children: ''
        }).then(function (data) {
          $scope.partyAuth = {
            parents: data.parents.map(function (party) {
              return { party: party };
            }),
            children: data.children.map(function (party) {
              return { party: party };
            })
          };

        }, function (res) {
          page.messages.addError({
            body: page.constants.message('network.authquery.error'),
            report: res,
          });
        });
      }
    };

    //

    var isAdmin = function () {
      return page.auth.hasAccess(page.permission.ADMIN, $scope.party);
    };

    $scope.isForeign = function () {
      if (!page.auth.user) return true;
      return $scope.party.id != page.auth.user.id;
    };

    $scope.showExtended = function () {
      return isAdmin();
    };

    //

    $scope.presetName = function (type, name, party) {
      if (angular.isString(party)) {
        return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.constants.message('auth.' + type + '.' + name, party);
      } else {
        return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
      }
    };

    //

    var userExists = function (list) {
      if (!page.auth.user) return false;
      var user = page.auth.user.id;

      /* you always exist on your own page */
      return $scope.party.id <= 0 || $scope.party.id === user ||
        list.some(function (a) {
          return a.party.id === user;
        });
    };

    $scope.canGrant = function () {
      return !($scope.party.institution || userExists($scope.partyAuth.parents));
    };

    $scope.canApply = function () {
      return !userExists($scope.partyAuth.children);
    };

    $scope.grant = function () {
      page.$location.url(page.auth.user.editRoute('grant'));
      var remove = page.events.listen(page.$rootScope, 'partyEditGrantForm-init', function (event, form, $thatScope) {
        remove();

        remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
          if (searchForm.principal != 'child') {
            return;
          }

          searchForm.selectFn($scope.party);
          form.scrollToFuture($scope.party);
          remove();
        });
      });
    };

    $scope.apply = function () {
      page.$location.url(page.auth.user.editRoute('apply'));
      var remove = page.events.listen(page.$rootScope, 'partyEditApplyForm-init', function (event, form, $thatScope) {
        remove();

        remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
          if (searchForm.principal == 'child') {
            return;
          }

          searchForm.selectFn($scope.party);
          form.scrollToFuture($scope.party);
          remove();
        });
      });
    };
  }
]);
