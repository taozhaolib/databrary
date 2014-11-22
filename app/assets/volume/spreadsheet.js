'use strict';

app.directive('spreadsheet', [
  'pageService', '$compile', '$templateCache',
  function (page, $compile, $templateCache) {
    function maybeInt(s) {
      var i = parseInt(s, 10);
      return isNaN(i) ? s : i;
    }
    function byNumber(a,b) {
      return a-b;
    }
    function byId(a,b) {
      return a.id-b.id;
    }
    function byType(a,b) {
      var ta = typeof a;
      var tb = typeof b;
      if (ta > tb) return 1;
      if (ta < tb) return -1;
      return a>b?1:a<b?-1:0;
    }
    function byMagic(a,b) {
      var na = parseFloat(a);
      var nb = parseFloat(b);
      if (na > nb) return 1;
      if (na < nb) return -1;
      return byType(a,b);
    }

    function stripPrefix(s, prefix) {
      return s.startsWith(prefix) ? s.substr(prefix.length) : undefined;
    }

    /* autovivification */
    function arr(a, f) {
      if (f in a)
        return a[f];
      else
        return (a[f] = []);
    }

    function obj(a, f) {
      if (f in a)
        return a[f];
      else
        return (a[f] = {});
    }

    function inc(a, f) {
      if (f in a)
        return a[f] ++;
      else {
        a[f] = 1;
        return 0;
      }
    }

    function parseInfo(id) {
      if (id === undefined)
        return;
      var s = id.split('_');
      var info = { t: s[0] };
      if (s.length > 1 && isNaN(info.i = parseInt(s[1], 10)))
        return;
      switch (info.t) {
        case 'rec':
          if (3 in s) {
            info.n = parseInt(s[2], 10);
            info.m = parseInt(s[3], 10);
          } else {
            info.n = 0;
            info.m = parseInt(s[2], 10);
          }
          break;
        case 'add':
        case 'more':
          info.c = parseInt(s[2], 10);
          break;
        case 'metric':
          info.m = info.i;
          delete info.i;
          break;
        case 'category':
          info.c = info.i;
          delete info.i;
          break;
      }
      return info;
    }

    var noCategory = {
      id: 0,
      name: 'record',
      not: 'No record',
      template: [page.constants.metricName.ident.id]
    };
    Object.freeze(noCategory);

    function getCategory(c) {
      return c != 0 ? page.constants.category[c] : noCategory; // jshint ignore:line
    }

    var pseudoMetrics = {
      id: {
        id: 'id',
        name: 'id',
        display: ' ',
        type: 'number',
        classification: page.classification.PUBLIC
      },
      age: {
        id: 'age',
        name: 'age',
        type: 'number',
        classification: page.classification.SHARED
      },
    };
    page.constants.deepFreeze(pseudoMetrics);

    function getMetric(m) {
      return pseudoMetrics[m] || page.constants.metric[m];
    }

    var selectStyles = document.head.appendChild(document.createElement('style')).sheet;

    var controller = [
      '$scope', '$element', '$attrs',
      function ($scope, $element, $attrs) {

        var volume = $scope.volume;
        $scope.page = page;

        var editing = $scope.editing = $attrs.edit !== undefined;
        var top = $scope.top = 'top' in $attrs;
        var id = $scope.id = $attrs.id || (top ? 'sst' : 'ss');

        /*
         * We use the following types of data structures:
         *   Row = index of slot in slots and rows (i)
         *   Data[Row] = scalar value (array over Row)
         *   Slot_id = Database id of container
         *   Segment = standard time range (see type service)
         *   Record_id = Database id of record
         *   Category_id = Database id of record category (c)
         *   Count = index of record within category for slot (n)
         *   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
         */

        var slots = []; // [Row] = Slot
        angular.forEach(volume.containers, function (s) {
          if (top !== !s.top) // jshint ignore:line
            slots.push(s);
        });

        var count = slots.length;
        var order = Object.keys(slots); // Permutation Array of Row in display order

        var records = {}; // [Category_id][Metric_id][Count] :: Data
        var counts = new Array(count); // [Row][Category_id] :: Count
        var recordCols = [], // [] Array over records :: {category: Category_id, metrics[]: Array of Metric_id}
            metricCols = []; // [] Array over metrics :: {category: Category, metric: Metric} (flattened version of recordCols)
        var depends = {}; // [Record_id][Row] :: Count

        var rows = new Array(count); // [Row] :: DOM Element tr

        var tbody = $element[0].getElementsByClassName("spreadsheet-tbody")[0];

        function parseId(el) {
          var info = parseInfo(stripPrefix(el.id, id+'-'));
          if (!info)
            return info;
          if ('m' in info) {
            info.c = (info.category = (info.col = metricCols[info.m]).category).id;
            info.metric = info.col.metric;
          } else if ('c' in info)
            info.category = getCategory(info.c);
          if ('i' in info) {
            info.slot = slots[info.i];
            if ('n' in info)
              info.record = volume.records[info.r = records[info.c].id[info.n][info.i]];
          }
          return info;
        }

        ///////////////////////////////// Populate data structures 

        /* Fill all Data values for Row i */
        function populateSlot(i) {
          var slot = slots[i];
          
          var r, n;
          function populateMeasure(m, v) {
            arr(arr(r, m), n)[i] = v;
          }
          var count = counts[i] = {};

          for (var ri = 0; ri < slot.records.length; ri ++) {
            var record = slot.records[ri].record;
            /* temporary workaround for half-built volume inclusions: */
            if (!record) continue;
            var c = record.category || 0;

            /* populate depends: */
            if (record.id in depends) {
              /* skip duplicates: */
              if (i in depends[record.id])
                continue;
            } else
              depends[record.id] = {};

            /* populate records: */
            if (c in records)
              r = records[c];
            else
              r = records[c] = {id: []};

            /* determine Count: */
            n = inc(count, c);

            /* populate measures: */
            populateMeasure('id', record.id);
            if (!editing && 'age' in slot.records[ri])
              populateMeasure('age', slot.records[ri].age);
            for (var m in record.measures)
              populateMeasure(m, record.measures[m]);

            depends[record.id][i] = n;
          }
        }

        /* Fill metricCols and recordCols from records */
        function populateCols() {
          metricCols = [];
          $scope.recordCols = recordCols = Object.keys(records).sort(byNumber).map(function (c) {
            var category = getCategory(c);
            if (editing)
              category.template.forEach(function (m) {
                arr(records[c], m);
              });
            var metrics = Object.keys(records[c]).map(maybeInt).sort(byType);
            metrics.pop(); // remove 'id' (necessarily last)
            metrics = metrics.map(getMetric);
            /* add back the 'id' column first if needed */
            if (!metrics.length || editing && !(metrics.length === 1 && metrics[0].options))
              metrics.unshift(pseudoMetrics.id);
            var si = metricCols.length;
            metricCols.push.apply(metricCols, metrics.map(function (m) {
              return {
                category: category,
                metric: m,
                sortable: m !== pseudoMetrics.id || metrics.length === 1
              };
            }));
            var l = metrics.length;
            metricCols[si].first = metricCols[si+l-1].last = l;
            return {
              category: category,
              metrics: metrics,
              start: si
            };
          });
          $scope.metricCols = metricCols;
        }

        /* Call all populate functions */
        function populate() {
          records = {};
          depends = {};
          for (var i = 0; i < count; i ++)
            populateSlot(i);
          populateCols();
          generate();
        }

        ///////////////////////////////// Generate HTML
        
        /* Find the text content of cell c with element t */
        function setCell(c, t) {
          var el = c.lastChild;
          if (el && el.nodeType === 3)
            c.replaceChild(t, el);
          else
            c.appendChild(t);
        }

        /* Add or replace the text contents of cell c for measure/type m with value v */
        function generateText(c, m, v, assumed) {
          if (m === 'consent') {
            var cn = page.constants.consent[v || 0];
            c.className = cn + ' consent icon hint-consent-' + cn;
            v = '';
          } else if (v === undefined) {
            c.classList.add('blank');
            v = assumed || '';
          } else {
            c.classList.remove('blank');
            if (m === 'id') {
              c.className = 'icon ' + (editing ? 'trash' : 'bullet');
              v = '';
            } else if (m === 'age')
              v = page.display.formatAge(v);
          }
          return setCell(c, document.createTextNode(v));
        }

        /* Add a td element to tr r with value c and id i */
        function generateCell(r, m, v, i, assumed) {
          var td = r.appendChild(document.createElement('td'));
          if (v === null)
            td.className = 'null';
          else {
            generateText(td, m, v, assumed);
            td.id = i;
          }
          return td;
        }

        /* Add all the measure tds to row i for count n, record r */
        function generateRecord(row, i, col, edit, n) {
          var c = col.category.id;
          var t = counts[i][c] || 0;
          var l = col.metrics.length;
          if (!l)
            return;
          var r = records[c];
          if (n === undefined ? t !== 1 : n >= t) {
            var td = row.appendChild(document.createElement('td'));
            td.setAttribute("colspan", l);
            if (n === undefined && t > 1) {
              td.appendChild(document.createTextNode(t + " " + col.category.name + "s"));
              td.className = 'more';
              td.id = id + '-more_' + i + '_' + c;
              for (n = 0; n < t; n ++)
                td.classList.add('ss-rec_' + r.id[n][i]);
            } else {
              td.className = 'null';
              if (!n || n === t) {
                if (!n)
                  td.appendChild(document.createTextNode(col.category.not));
                else if (edit)
                  td.appendChild(document.createTextNode("add " + col.category.name));
                if (edit) {
                  td.className = 'null add';
                  td.id = id + '-add_' + i + '_' + c;
                }
              }
            }
            return;
          }
          var ms = col.metrics;
          var b = id + '-rec_' + i + '_';
          if (n === undefined)
            n = 0;
          else
            b += n + '_';
          for (var mi = 0; mi < l; mi ++) {
            var m = ms[mi].id;
            var v = r[m][n] && r[m][n][i];
            var cell = generateCell(row, m, v, b + (col.start+mi), ms[mi].assumed);
            if (v !== null) {
              var ri = 'ss-rec_' + r.id[n][i];
              cell.classList.add(ri);
              cell.classList.add(ri + '_' + m);
            }
          }
        }

        /* Fill out rows[i]. */
        function generateRow(i) {
          var slot = slots[i];
          var stop = slot.id === volume.top.id;
          var row = document.createElement('tr');
          if (rows[i] && rows[i].parentNode)
            rows[i].parentNode.replaceChild(row, rows[i]);
          rows[i] = row;
          var cell;
          row.id = id + '_' + i;
          row.data = i;
          if (editing && stop)
            row.className = 'top';

          var name = slot.name;
          if (stop && !name)
            name = page.constants.message('materials.top');
          cell = generateCell(row, 'name', name, id + '-name_' + i);
          var a;
          if (editing && !stop) {
            a = cell.insertBefore(document.createElement('a'), cell.firstChild);
            a.className = 'trash icon';
            $(a).on('click', function (event) {
              $scope.$apply(function () {
                removeSlot(cell, i, slot);
              });
              event.stopPropagation();
            });
          }
          a = cell.insertBefore(document.createElement('a'), cell.firstChild);
          a.setAttribute('href', editing ? slot.editRoute() : slot.route());
          a.className = "go icon hint-object-play";

          if (!slot.top)
            generateCell(row, 'date', slot.date, id + '-date_' + i);
          generateCell(row, 'consent', slot.consent, id + '-consent_' + i);
          for (var ci = 0; ci < recordCols.length; ci ++)
            generateRecord(row, i, recordCols[ci], editing && !stop);
        }

        /* Update all age displays. */
        function regenerateAges() {
          for (var mi = 0; mi < metricCols.length; mi ++) {
            var m = metricCols[mi];
            if (m.metric.id !== 'age')
              continue;
            var c = m.category.id;
            var r = records[c][m.metric.id];
            if (expandedCat === c && counts[expanded][c] > 1)
              for (var n = 0; n < counts[expanded][c]; n ++) {
                if (n in r) generateText(
                    document.getElementById(id + '-rec_' + expanded + '_' + n + '_' + mi),
                    'age', r[n][expanded]);
              }
            if (!(0 in r))
              return;
            r = r[0];
            var post = '_' + mi;
            for (var i = 0; i < count; i ++) {
              if (counts[i][c] === 1) generateText(
                document.getElementById(id + '-rec_' + i + post),
                'age', r[i]);
            }
          }
        }

        $scope.$on('displayService-toggleAge', regenerateAges);

        /* Generate all rows. */
        function generate() {
          for (var i = 0; i < count; i ++)
            generateRow(i);
          fill();
        }

        ///////////////////////////////// Place DOM elements
        
        /* Place all rows into spreadsheet. */
        function fill() {
          collapse();
          for (var o = 0; o < count; o ++) {
            var i = order[o];
            tbody.appendChild(rows[i]);
          }
        }

        /* Populate order based on compare function applied to values. */
        function sort(values, compare) {
          if (!values)
            return;
          if (!compare)
            compare = byMagic;
          order.sort(function (i, j) {
            return compare(values[i], values[j]);
          });
        }

        /* Sort by values, called name. */
        function sortBy(key, values) {
          if ($scope.currentSort === key) {
            $scope.currentSortDirection = !$scope.currentSortDirection;
            order.reverse();
          } else {
            sort(values);
            $scope.currentSort = key;
            $scope.currentSortDirection = false;
          }
          fill();
        }

        /* Sort by one of the container columns. */
        function sortBySlot(f, $event) {
          sortBy(f, slots.map(function (s) { return s[f]; }));

          //toggle appearance of sort arrows on clickSlot
          var el = $event.target;
          el.classList.remove("sortable");
          if(el.classList.contains("sort-asc")){
            el.classList.remove("sort-asc");
            el.classList.add("sort-desc");
          } else {
            el.classList.remove("sort-desc");
            el.classList.add("sort-asc");
          }

        }

        /* Sort by Category_id c's Metric_id m */
        function sortByMetric(col) {
          sortBy(col, records[col.category.id][col.metric.id][0]);
        }

        ///////////////////////////////// Backend saving
        
        function saveError(cell, res) {
          cell.classList.remove('saving');
          cell.classList.add('error');
          page.messages.addError({
            body: 'error',
            report: res
          });
        }

        function createSlot(cell) {
          cell.classList.remove('error');
          cell.classList.add('saving');
          volume.createContainer({top:top}).then(function (slot) {
            if (!('records' in slot))
              slot.records = [];
            var i = (count = slots.push(slot))-1;
            order.push(i);
            populateSlot(i);
            generateRow(i);
            tbody.appendChild(rows[i]);
            cell.classList.remove('saving');
          }, saveError.bind(null, cell));
        }

        function saveSlot(cell, info, v) {
          var data = {};
          data[info.t] = v === undefined ? '' : v;
          if (info.slot[info.t] === data[info.t])
            return;
          cell.classList.remove('error');
          cell.classList.add('saving');
          return info.slot.save(data).then(function () {
            generateText(cell, info.t, info.slot[info.t]);
            cell.classList.remove('saving');
          }, saveError.bind(null, cell));
        }

        function removeSlot(cell, i, slot) {
          /* assuming we have a container */
          cell.classList.remove('error');
          cell.classList.add('saving');
          return slot.remove().then(function (done) {
            cell.classList.remove('saving');
            if (!done)
              return page.messages.add({
                body: page.constants.message('slot.remove.notempty'),
                type: 'red',
                countdown: 5000
              });
            unedit();
            collapse();
            rows[i].parentNode.removeChild(rows[i]);
            count --;
            slots.splice(i, 1);
            counts.splice(i, 1);
            rows.splice(i, 1);
            order.remove(i);
            order = order.map(function (j) {
              return j - (j > i);
            });
            populate();
          }, saveError.bind(null, cell));
        }

        function saveMeasure(cell, record, metric, v) {
          if (record.measures[metric.id] === v)
            return;
          cell.classList.remove('error');
          cell.classList.add('saving');
          return record.measureSet(metric.id, v).then(function (rec) {
            var rcm = records[rec.category || 0][metric.id];
            angular.forEach(depends[record.id], function (n, i) {
              arr(rcm, n)[i] = v;
              /* TODO age may have changed... not clear how to update. */
            });

            var l = tbody.getElementsByClassName('ss-rec_' + record.id + '_' + metric.id);
            for (var li = 0; li < l.length; li ++)
              generateText(l[li], metric.id, v, metric.assumed);
            cell.classList.remove('saving');
          }, saveError.bind(null, cell));
        }

        function setRecord(cell, info, record) {
          cell.classList.remove('error');
          cell.classList.add('saving');
          var add = function () {
            if (record)
              return info.slot.addRecord(record);
            else if (record !== null)
              return info.slot.newRecord(info.c || '');
          };
          var act = info.record ?
            info.slot.removeRecord(info.record).then(add) :
            add();

          return act.then(function (record) {
            var r, m, rcm;
            if (record) {
              r = record.id;
              if (!('n' in info))
                info.n = inc(counts[info.i], info.c);

              for (m in records[info.c]) {
                rcm = records[info.c][m];
                var v = m in record ? record[m] : record.measures[m];
                if (v === undefined) {
                  if (info.n in rcm)
                    delete rcm[info.n][info.i];
                } else
                  arr(rcm, info.n)[info.i] = v;
              }
              /* TODO this may necessitate regenerating column headers */
            } else {
              var t = --counts[info.i][info.c];
              for (m in records[info.c]) {
                rcm = records[info.c][m];
                for (var n = info.n+1; n < rcm.length; n ++)
                  arr(rcm, n-1)[info.i] = arr(rcm, n)[info.i];
                if (t in rcm)
                  delete rcm[t][info.i];
              }
            }

            if (info.record)
              delete depends[info.r][info.i];
            if (record)
              obj(depends, r)[info.i] = info.n;

            cell.classList.remove('saving');
            collapse();
            generateRow(info.i);
            if (info.n)
              expand(info);
            return record;
          }, saveError.bind(null, cell));
        }

        ///////////////////////////////// Interaction
        
        var expanded, expandedCat;

        /* Collapse any expanded row. */
        function collapse() {
          if (expanded === undefined)
            return;
          var i = expanded;
          expanded = expandedCat = undefined;
          var row = rows[i];
          row.classList.remove('expand');
          var el;
          if (!((el = row.nextSibling) && el.data === i))
            return false;
          do {
            tbody.removeChild(el);
          } while ((el = row.nextSibling) && el.data === i);

          for (el = row.firstChild; el; el = el.nextSibling)
            el.removeAttribute("rowspan");

          return true;
        }

        /* Expand (or collapse) a row */
        function expand(info) {
          if (expanded === info.i && expandedCat === info.c) {
            if (info.t === 'more')
              collapse();
            return;
          }
          collapse();

          expanded = info.i;
          expandedCat = info.c;
          var row = rows[expanded];
          row.classList.add('expand');

          var max = counts[expanded][expandedCat];
          var edit = editing && slots[expanded].id !== volume.top.id;
          if (edit)
            max ++;
          if (max <= 1)
            return;
          var col = recordCols.find(function (col) {
            return col.category.id === expandedCat;
          });
          var next = row.nextSibling;
          var el;
          var start = counts[expanded][expandedCat] === 1;
          for (var n = +start; n < max; n ++) {
            el = tbody.insertBefore(document.createElement('tr'), next);
            el.data = expanded;
            el.className = 'expand';
            generateRecord(el, expanded, col, edit, n);
          }

          max += !start;
          for (el = row.firstChild; el; el = el.nextSibling) {
            var info = parseId(el);
            if (!info || info.c !== expandedCat)
              el.setAttribute("rowspan", max);
          }
        }

        function save(cell, type, value) {
          var info = parseId(cell);
          if (value === '')
            value = undefined;
          else switch (type) {
            case 'consent':
              value = parseInt(value, 10);
              break;
            case 'record':
              var v;
              if (value === 'new')
                setRecord(cell, info);
              else if (value === 'remove')
                setRecord(cell, info, null);
              else if ((v = stripPrefix(value, 'add_'))) {
                var u = v.indexOf('_');
                var m = page.constants.metric[v.slice(0,u)];
                v = v.slice(u+1);
                setRecord(cell, info).then(function (r) {
                  if (r)
                    saveMeasure(cell, r, m, v);
                });
              } else if (!isNaN(v = parseInt(value, 10))) {
                if (v !== info.r)
                  setRecord(cell, info, volume.records[v]);
                else
                  edit(cell, info, true);
              }
              return;
            case 'metric':
              if (value !== undefined) {
                arr(records[info.c], value);
                populateCols();
                generate();
              }
              return;
            case 'category':
              if (value !== undefined) {
                arr(obj(records, value), 'id');
                populateCols();
                generate();
              }
              return;
          }

          switch (info.t) {
            case 'name':
            case 'date':
            case 'consent':
              return saveSlot(cell, info, value);
            case 'rec':
              return saveMeasure(cell, info.record, info.metric, value);
          }
        }

        var editScope = $scope.$new(true);
        editScope.page = page;
        var editInput = editScope.input = {};
        var editCellTemplate = $compile($templateCache.get('volume/spreadsheetEditCell.html'));
        var editCell;

        function unedit(event) {
          var edit;
          if (!(edit = editCell))
            return;
          editCell = undefined;
          $(edit).children('[name=edit]').off();
          var cell = edit.parentNode;
          if (!cell)
            return;
          cell.removeChild(edit);
          cell.classList.remove('editing');
          page.tooltips.clear();

          if (event)
            save(cell, editScope.type, editInput.value);
        }
        editScope.unedit = unedit;

        function edit(cell, info, alt) {
          var m;
          if (info.slot && info.slot.id === volume.top.id)
            return;
          switch (info.t) {
            case 'name':
              editScope.type = 'text';
              editInput.value = info.slot.name;
              break;
            case 'date':
              editScope.type = 'date';
              editInput.value = info.slot.date;
              break;
            case 'consent':
              editScope.type = 'consent';
              editInput.value = (info.slot.consent || 0) + '';
              break;
            case 'rec':
            if (info.metric.id === 'id') {
              setRecord(cell, info, null);
              return;
            }
            if (!info.col.first || alt) {
              m = info.metric.id;
              /* we need a real metric here: */
              if (typeof m !== 'number')
                return;
              editInput.value = volume.records[info.r].measures[m];
              if (editInput.value === undefined)
                editInput.value = '';
              if (info.metric.options) {
                editScope.type = 'select';
                editScope.options = [''].concat(info.metric.options);
              } else if (info.metric.long)
                editScope.type = 'long';
              else
                editScope.type = info.metric.type;
              break;
            }
              /* falls through */
            case 'add':
              var c = info.category;
              if ('r' in info)
                editInput.value = info.r + '';
              else
                editInput.value = 'remove';
              editScope.type = 'record';
              editScope.options = {
                'new': 'Create new ' + c.name,
                'remove': c.not
              };
              angular.forEach(volume.records, function (r, ri) {
                if ((r.category || 0) === c.id && (!(ri in depends && info.i in depends[ri]) || ri === editInput.value))
                  editScope.options[ri] = r.displayName;
              });
              /* detect special cases: singleton or unitary records */
              for (var mi in records[c.id]) {
                var mm = page.constants.metric[mi];
                if (!m)
                  m = mm;
                else if (mm) {
                  m = null;
                  break;
                }
              }
              if (m === undefined && Object.keys(editScope.options).length > 2)
                /* singleton: id only, existing record(s) */
                delete editScope.options['new'];
              else if (m && m.options) {
                /* unitary: single metric with options */
                delete editScope.options['new'];
                m.options.forEach(function (o) {
                  for (var ri in volume.records) {
                    var r = volume.records[ri];
                    if ((r.category || 0) === c.id && r.measures[m.id] === o)
                      return;
                  }
                  editScope.options['add_'+m.id+'_'+o] = o;
                });
              }
              break;
            case 'category':
              editScope.type = 'metric';
              editInput.value = undefined;
              editScope.options = [];
              angular.forEach(page.constants.metric, function (m, mi) {
                if (!(mi in records[info.c]))
                  editScope.options.push(m);
              });
              editScope.options.sort(byId);
              break;
            case 'head':
              editScope.type = 'category';
              editInput.value = undefined;
              editScope.options = [];
              angular.forEach(page.constants.category, function (c, ci) {
                if (!(ci in records))
                  editScope.options.push(c);
              });
              editScope.options.sort(byId);
              if (!(noCategory.id in records))
                editScope.options.push(noCategory);
              break;
            default:
              return;
          }
          var e = editCellTemplate(editScope, function (e) {
            cell.insertBefore(editCell = e[0], cell.firstChild);
            cell.classList.add('editing');
          });
          e.on('click', function ($event) {
            /* prevent other ng-click handlers from taking over */
            $event.stopPropagation();
          });

          page.tooltips.clear();
          page.$timeout(function () {
            var input = e.children('[name=edit]');
            input.focus();
            /* chrome produces spurious change events on date fields, so we rely on key-enter instead. */
            if (editScope.type !== 'date')
              input.one('change', $scope.$lift(unedit));
          });
        }

        function unselect() {
          while (selectStyles.cssRules.length)
            selectStyles.deleteRule(0);

          unedit();
        }

        $scope.$on('$destroy', unselect);

        function select(cell, info) {
          unselect();

          expand(info);

          if (info.t === 'rec') {
            var cl = cell.classList;
            for (var ci = 0; ci < cl.length; ci ++) {
              var c = cl[ci];
              if (c.startsWith('ss-rec_'))
                selectStyles.insertRule('.' + c + '{background-color:' +
                  (!c.contains('_', 7) ? '#f3f1bf' : '#e8e47f') + 
                  ';\n text-}', selectStyles.cssRules.length);
            }
          }

          if (editing)
            edit(cell, info);
        }

        $scope.click = function (event) {
          var el = event.target;
          if (el.tagName !== 'TD')
            return;
          var info = parseId(el);
          if (!info)
            return;

          select(el, info);
          if ('m' in info && metricCols[info.m].metric.id === 'age')
            page.display.toggleAge();
        };

        $scope.clickSlot = function ($event, t) {
          if (t)
            sortBySlot(t, $event);
          else
            unselect();
        };
        $scope.clickAdd = function ($event) {
          unselect();
          edit($event.target, {t:'head'});
        };
        $scope.clickCategoryAdd = function ($event, col) {
          unselect();
          if (editing)
            edit($event.target.parentNode, {t:'category',c:col.category.id});
        };
        $scope.clickMetric = function (col) {
          if (col.sortable)
            sortByMetric(col);
        };
        $scope.clickNew = function ($event) {
          createSlot($event.target);
        };

        ///////////////////////////////// main

        $scope.refresh = function() {
          unedit();
          collapse();
          populate();
        };

        populate();
      }
    ];

    return {
      restrict: 'E',
      scope: {
        volume: '=',
      },
      templateUrl: 'volume/spreadsheet.html',
      controller: controller,
    };
  }
]);
