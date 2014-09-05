'use strict';

module.factory('tooltipService', [
  '$rootScope', '$timeout', function ($rootScope, $timeout) {

    var $doc = $(document);

    var padW = 20;
    var padH = 15;

    var defaults = {
      cls: '',
      style: {},
      type: 'blue',
      visible: false,
      live: false,
      delay: 500,
    };

    var sequence = 0;

    function Tooltip(init) {
      this.id = init.id || 'tooltip-' + sequence++;
      angular.extend(this, defaults, init);
      Tooltip.list[this.id] = this;
      target(this);
    }

    Tooltip.list = {};

    Tooltip.prototype.remove = function () {
      removeEvents(this);
      delete Tooltip.list[this.id];
    };

    Tooltip.prototype.show = function (event) {
      position(this, event.clientX, event.clientY);
      this.visible = true;
    };

    Tooltip.prototype.hide = function () {
      position(this);
      this.visible = false;
    };

    function position(tooltip, locx, locy) {
      tooltip.position = [];

      if (arguments.length === 1)
        return;

      if (!locx)
        locx = tooltip.$target.offset().left;
      if (!locy)
        locy = tooltip.$target.offset().top;

      var $w = $(window);
      var center = {
        left: locx,
        top: locy,
        right: $w.width() - locx,
        bottom: $w.height() - locy
      };

      var $e = $('#' + tooltip.id);

      if (center.left > center.right) {
        tooltip.style.left = (locx + $(window).scrollLeft() - $e.outerWidth() + padW) + 'px';
        tooltip.position.push('left');
      } else {
        tooltip.style.left = (locx + $(window).scrollLeft() - padW) + 'px';
        tooltip.position.push('right');
      }

      if (center.top > center.bottom) {
        tooltip.style.top = (locy + $(window).scrollTop() - $e.outerHeight() - padH) + 'px';
        tooltip.position.push('top');
      } else {
        tooltip.style.top = (locy + $(window).scrollTop() + padH) + 'px';
        tooltip.position.push('bottom');
      }
    }

    var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'];
    function getTargetEvents(tooltip) {
      if (!tooltip.$target) {
        return [];
      }

      var namespace = '.tooltipTarget-' + tooltip.id;

      if (!angular.isString(tooltip.$target) && focusElements.indexOf(tooltip.$target.prop('tagName')) >= 0) {
        return [
            'focusin' + namespace,
            'focusout' + namespace
        ];
      }

      return [
          'mouseenter' + namespace,
          'mouseleave' + namespace
      ];
    }

    function removeEvents(tooltip) {
      if (tooltip.$target) {
        if (tooltip.live) {
          $doc.off(getTargetEvents(tooltip).join(' '), tooltip.$target);
        }
        else {
          tooltip.$target.unbind(getTargetEvents(tooltip).join(' '));
        }
      }
    }

    function target(tooltip) {
      if (!(tooltip.id in Tooltip.list))
	return;

      removeEvents(tooltip);

      var $target = tooltip.$target;

      if (!tooltip.live && $target.length === 0)
	return tooltip.remove();

      var events = getTargetEvents(tooltip);

      var timeout;

      if (tooltip.live) {
        $doc.on(events[0], tooltip.$target, function (event) {
	  var target = $(event.target);
          timeout = $timeout(function () {
	    if (target.is(tooltip.$target)) // may have changed
	      tooltip.show(event);
          }, tooltip.delay);
        });

        $doc.on(events[1], tooltip.$target, function () {
          $rootScope.$apply(function () {
            $timeout.cancel(timeout);
            tooltip.hide();
          });
        });
      } else {
        $target.bind(events[0], function (event) {
          timeout = $timeout(function () {
            tooltip.show(event);
          }, tooltip.delay);
        });

        $target.bind(events[1], function () {
          $rootScope.$apply(function () {
            $timeout.cancel(timeout);
            tooltip.hide();
          });
        });
      }

      tooltip.hide();

      return tooltip;
    }

    //

    $rootScope.$watch(function () {
      angular.forEach(Tooltip.list, function (tooltip) {
        if (!angular.isString(tooltip.$target) &&
	  (document.contains ? !document.contains(tooltip.$target[0]) :
	   tooltip.$target.closest(document.documentElement).length === 0)) {
	  tooltip.remove();
        }
      });
    });

    Tooltip.clear = function () {
      angular.forEach(Tooltip.list, function (tooltip) {
        tooltip.hide();
      });
    };

    $rootScope.$on('$routeChangeStart', Tooltip.clear);

    //

    return Tooltip;
  }
]);
