'use strict';

module.factory('messageService', [
  '$timeout', '$sanitize', '$sce', 'constantService',
  function ($timeout, $sanitize, $sce, constants) {

    var defaults = {
      type: 'blue',
      closeable: false,
      countdown: false,
    };

    var sequence = 0;
    var byBody = {};

    function Message(init) {
      this.id = init.id || 'message-' + sequence++;
      angular.extend(this, defaults, init);

      /* suppress duplicate messages */
      if (this.body in byBody)
	return;

      Message.list[this.id] = this;
      byBody[this.body] = this;

      if (this.countdown)
	countdown(this);
    }

    Message.list = {};

    Message.prototype.remove = function () {
      countdownClear(this);
      delete Message.list[this.id];
      delete byBody[this.body];
    };

    function countdownClear(message) {
      if (message.countdownTimer) {
        $timeout.cancel(message.countdownTimer);
	message.countdownTimer = undefined;
      }
    }

    function countdown(message) {
      countdownClear(message);

      if (!message.countdown)
        return;

      message.countdownTimer = $timeout(function () {
        message.remove();
      }, message.countdown);
    }

    Message.add = function (message) {
      return new Message(message);
    };

    /* NB: modifies message */
    Message.addError = function (message) {
      message.countdown = false;
      message.closeable = true;
      message.type = 'red';

      var body = constants.message('error.prefix') + ' ' + $sce.getTrustedHtml(message.body);

      if (message.report) {
        message.errors = message.report.data;
        message.status = message.report.status;
        message.url = message.report.config.url;
      }

      if (!message.errors) {
        body += ' ' + constants.message('error.suffix');
      } else if (angular.isString(message.errors)) {
        message.fn = function () {
	  var doc = document.open('text/html', 'replace');
	  doc.write(message.errors);
	  doc.close();
	};
        body += ' ' + constants.message('error.view');
      } else if (angular.isObject(message.errors)) {
        var moreBody = '';
        var messageBody = '';

        if (angular.isObject(message.errors)) {
          angular.forEach(message.errors, function (errorArray, field) {
            moreBody += '<dl class="comma"><dt>' + (field || '') + '</dt><dd>' + errorArray.map($sanitize).join('</dd><dd>') + '</dd></dl>';
            messageBody += 'Field "' + (field || 'validation') + '":\n' + errorArray.join('\n') + '\n\n';
          });
        }

	var refreshMsg = false;
        if (message.status) {
          messageBody = 'Status:\n' + message.status + '\n\n' + messageBody;
	  refreshMsg = message.status == 409;
        }

        if (messageBody) {
          body += ' ' + constants.message('error.report', encodeURIComponent(constants.message('error.report.subject', message.status || 'Unknown', message.url || 'Location unknown')), encodeURIComponent(constants.message('error.report.body', messageBody))) + moreBody;
	  if(refreshMsg){
	      body += "<br>" + constants.message('app.reload');
	  }
        }
      }

      delete message.report;
      delete message.errors;
      delete message.status;
      delete message.url;
      message.body = $sce.trustAsHtml(body);

      return new Message(message);
    };

    return Message;
  }
]);
