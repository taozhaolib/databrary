'use strict';

module.directive('partyEditProfileForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.partyEditProfileForm;

      form.data = {};
      form.authors = [];

      form.saveFn = undefined;
      form.resetFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      //

      form.init = function (party) {
	form.party = form.party || party;
	form.data = {
	  name: party.name,
	  orcid: party.orcid,
	  affiliation: party.affiliation,
	  avatar: party.avatar,
	  url: party.url
	};
      };

      //

      form.save = function () {
	if (angular.isFunction(form.saveFn)) {
	  form.saveFn(form);
	}

	var fd, upload;
	if (angular.isObject(form.data.avatar)) {
	  fd = new FormData();

	  fd.append('avatar', form.data.avatar[0]);
	  form.data.avatar = undefined;

	  for (var prop in form.data) {
	    if (form.data.hasOwnProperty(prop) && angular.isDefined(form.data[prop]) && form.data[prop] !== null) {
	      fd.append(prop, form.data[prop]);
	    }
	  }

	  upload = form.messages.add({
	    type: 'yellow',
	    body: page.constants.message('party.edit.avatar.upload', page.constants.message('avatar')),
	  });
	} else
	  fd = form.data;
	
	form.party.save(fd)
	  .then(function (res) {
	    form.validator.server({});

	    form.messages.add({
	      type: 'green',
	      countdown: 3000,
	      body: page.constants.message('party.edit.profile.success'),
	    });

	    if (upload) {
	      form.data.avatar = res.data.avatar;
	      form.messages.remove(upload);
	    }

	    if (page.auth.user.id === form.party.id) {
	      page.auth.user.name = res.name;
	    }

	    if (angular.isFunction(form.successFn)) {
	      form.successFn(form, res);
	    }

	    form.$setPristine();

	    if (upload)
	      form.avatarUrl = form.getAvatar();
	  }, function (res) {
	    form.validator.server(res);
	    page.display.scrollTo(form.$element);

	    if (upload)
	      form.messages.remove(upload);

	    if (angular.isFunction(form.errorFn)) {
	      form.errorFn(form, res);
	    }
	  });
      };

      form.resetAll = function(force){
	if(force || confirm(page.constants.message('navigation.confirmation'))){
	  page.$route.reload();
	  return true;
	}
	return false;
      };


      //

      form.validator.client({
	name: {
	  tips: page.constants.message('party.edit.name.help')
	},
	affiliation: {
	  tips: page.constants.message('party.edit.affiliation.help')
	},
	orcid: {
	  tips: page.constants.message('party.edit.orcid.help')
	},
	url: {
	  tips: page.constants.message('party.edit.url.help')
	}
      }, true);

      //

      form.getAvatar = function () {
	return form.party && form.party.avatarRoute(undefined, Date.now());
      };

      page.$timeout(function () {
	form.avatarUrl = form.getAvatar();
      });

      //

      page.events.talk('partyEditProfileForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'partyEditProfileForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
