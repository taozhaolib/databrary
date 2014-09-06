'use strict';

module.directive('partyEditProfileForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.partyEditProfileForm;

      form.data = {};
      form.authors = [];

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

	    form.$setPristine();

	    if (upload)
	      form.avatarUrl = form.getAvatar();
	  }, function (res) {
	    form.validator.server(res);
	    page.display.scrollTo(form.$element);

	    if (upload)
	      form.messages.remove(upload);
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

      var $float = $('.pep-float');
      var $floater = $('.pep-float-floater');
      $scope.scrollFn = page.display.makeFloatScrollFn($float, $floater, 24*1.5);
      page.$w.scroll($scope.scrollFn);
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
