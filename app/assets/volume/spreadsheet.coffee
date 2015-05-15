'use strict'

app.directive 'spreadsheet', [
  'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', '$compile', '$templateCache', '$timeout', '$document', '$location',
  (constants, display, messages, tooltips, styles, $compile, $templateCache, $timeout, $document, $location) ->
    maybeInt = (s) ->
      if isNaN(i = parseInt(s, 10)) then s else i
    byDefault = (a,b) -> +(a > b) || +(a == b) - 1
    byNumber = (a,b) -> a-b
    byType = (a,b) ->
      ta = typeof a
      tb = typeof b
      if ta != tb
        a = ta
        b = tb
      byDefault(a,b)
    byMagic = (a,b) ->
      if isNaN(d = a-b) then byDefault(a,b) else d
    bySortId = (a,b) ->
      (a.sort || a.id)-(b.sort || b.id)

    stripPrefix = (s, prefix) ->
      if s.startsWith(prefix) then s.substr(prefix.length)

    # autovivification
    arr = (a, f) ->
      if f of a then a[f] else a[f] = []
    obj = (a, f) ->
      if f of a then a[f] else a[f] = {}
    inc = (a, f) ->
      if f of a then a[f]++ else
        a[f] = 1
        0

    pseudoMetric =
      id:
        id: 'id'
        name: 'id'
        display: ' '
        type: 'number'
        release: constants.release.PUBLIC
        sort: -10000
        readonly: true
      name: # slot, asset
        id: 'name'
        name: 'name'
        display: ' '
        type: 'text'
        sort: -9000
      date: # slot
        id: 'date'
        name: 'test date'
        type: 'date'
        sort: -8000
      release: # slot
        id: 'release'
        name: 'release'
        type: 'release'
        sort: -7000
      classification: # asset
        id: 'classification'
        name: 'classification'
        type: 'classification'
        sort: -6000
        readonly: true
      excerpt: # asset
        id: 'excerpt'
        name: 'excerpt'
        sort: -5000
        readonly: true
      age: # record
        id: 'age'
        name: 'age'
        type: 'number'
        release: constants.release.EXCERPTS
        sort: constants.metric.birthdate + 0.5
        readonly: true
    constants.deepFreeze(pseudoMetric)
    getMetric = (m) ->
      pseudoMetric[m] || constants.metric[m]

    {
    restrict: 'E'
    scope: true
    templateUrl: 'volume/spreadsheet.html'
    controller: [
      '$scope', '$element', '$attrs',
      ($scope, $element, $attrs) ->
        Volume = $scope.volume

        Editing = $scope.editing = $attrs.edit != undefined
        Top = $scope.top = 'top' of $attrs
        Assets = 'assets' of $attrs
        ID = $scope.id = $attrs.id ? if Top then 'sst' else 'ss'
        Limit = $attrs.limit

        ###
        # We use the following types of data structures:
        #   Row = index of slot in slots and rows (i)
        #   Data[Row] = scalar value (array over Row)
        #   Slot_id = Database id of container
        #   Segment = standard time range (see type service)
        #   Record_id = Database id of record
        #   Category_id = Database id of record category (c)
        #   Count = index of record within category for slot (n)
        #   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
        ###

        ### jshint ignore:start #### fixed in jshint 2.5.7
        Slots = (container for containerId, container of Volume.containers when Top != !container.top) # [Row] = Slot
        ### jshint ignore:end ###

        Order = if Slots.length then [0..Slots.length-1] else [] # Permutation Array of Row in display order

        Data = {}                   # [Category_id][Metric_id][Count] :: Data
        Counts = new Array(Slots.length) # [Row][Category_id] :: Count
        Groups = []                 # [] Array over categories :: {category: Category, metrics[]: Array of Metric}
        Cols = []                   # [] Array over metrics :: {category: Category, metric: Metric} (flattened version of Groups)
        Depends = {}                # [Record_id][Row] :: Count

        Rows = new Array(Slots.length) # [Row] :: DOM Element tr

        TBody = $element[0].getElementsByTagName("tbody")[0]

        pseudoCategory =
          slot:
            id: 'slot'
            name: if Top then 'materials' else 'session'
            template: if Top then ['name'] else ['name', 'date', 'release']
            sort: -10000
            fixed: true
          0:
            id: 0
            name: 'record'
            not: 'No record'
            template: [constants.metricName.ID.id]
          asset:
            id: 'asset'
            name: 'file'
            not: 'No files'
            template: ['name', 'classification', 'excerpt']
            sort: 10000
            fixed: true
        constants.deepFreeze(pseudoCategory)
        getCategory = (c) ->
          pseudoCategory[c || 0] || constants.category[c]

        class Info
          # Represents everything we know about a specific cell.  Properties:
          #   cell: target TD element
          #   id: cell.id
          #   i: Row
          #   n: Count (index of count), optional [0]
          #   m: index into Cols
          #   cols: Groups element
          #   col: Cols element
          #   category: Category
          #   c: Category_id
          #   count: Count[i][c]
          #   metric: Metric
          #   row: Rows[i]
          #   slot: Slots[i]
          #   d: Data for id metric
          #   record: Record
          #   asset: Asset
          #   v: Data value

          constructor: (@cell) ->
            @parseId()
            return

          parseId: (i) ->
            return unless (if i? then @id = i else i = @id) and (i = stripPrefix(i, ID+'-'))
            s = i.split '_'
            return if s.length > 1 && isNaN(@i = parseInt(s[1], 10))
            switch @t = s[0]
              when 'rec'
                if 3 of s
                  @n = parseInt(s[2], 10)
                  @m = parseInt(s[3], 10)
                else
                  @m = parseInt(s[2], 10)
              when 'add', 'more'
                @c = s[2]
              when 'metric'
                @m = @i
                delete @i
              when 'category'
                @c = @i
                delete @i
              #when 'asset', 'class', 'excerpt'
              #  @c = 'asset'
              #  info.a = if 2 of s then parseInt(s[2], 10) else 0
            true

          properties =
            n: ->
              0
            id: ->
              @cell?.id
            cols: ->
              c = @c
              @cols = Groups.find (col) -> `col.category.id == c`
            col: ->
              Cols[@m]
            category: ->
              if (c = @col)?
                c.category
              else if this.hasOwnProperty('c')
                @category = getCategory(@c)
            c: ->
              @category?.id
            count: ->
              Counts[@i][@c] || 0
            metric: ->
              @col?.metric
            row: ->
              Rows[@i]
            slot: ->
              Slots[@i]
            d: ->
              Data[@c].id[@n]?[@i]
            record: ->
              Volume.records[@d]
            asset: ->
              @slot?.assets[@d]
            v: ->
              Data[@c][@m]?[@n]?[@i]

          property = (v, f) ->
            get: f
            set: (x) ->
              Object.defineProperty @, v,
                value: x
                writable: true
                configureable: true
                enumerable: true
              return

          for v, f of properties
            properties[v] = property(v, f)

          Object.defineProperties @prototype,
            properties

        parseId = (el) ->
          info = new Info(el)
          info if info.c

        ################################# Populate data structures 

        # Fill all Data values for Row i
        populateSlot = (i) ->
          slot = Slots[i]

          # r, n
          populateMeasure = (m, v) ->
            arr(arr(r, m), n)[i] = v
            return

          count = Counts[i] = {slot: 1}
          c = 'slot'
          r = Data[c]
          n = 0
          populateMeasure('id', slot.id)
          populateMeasure('name', slot.name)
          if !slot.top || slot.date
            populateMeasure('date', slot.date)
          if !slot.top || slot.release
            populateMeasure('release', slot.release)

          for rr in slot.records
            record = rr.record
            # temporary workaround for half-built volume inclusions:
            continue unless record
            c = record.category || 0

            # populate depends:
            if record.id of Depends
              # skip duplicates:
              continue if i of Depends[record.id]
            else
              Depends[record.id] = {}

            # populate records:
            r = if c of Data then Data[c] else Data[c] = {id:[]}

            # determine Count:
            n = inc(count, c)

            # populate measures:
            populateMeasure('id', record.id)
            if !Editing && 'age' of rr
              populateMeasure('age', rr.age)
            for m, v of record.measures
              populateMeasure(m, v)

            Depends[record.id][i] = n

          if Assets
            c = 'asset'
            r = Data[c]
            for assetId, asset of slot.assets
              n = inc(count, c)
              populateMeasure('id', asset.id)
              populateMeasure('name', asset.name)
              populateMeasure('classification', asset.release)
              populateMeasure('excerpt', asset.excerpt?)

          return

        # Fill Cols and Groups from records
        populateCols = ->
          Cols = []
          $scope.groups = Groups = Object.keys(Data).map(getCategory).sort(bySortId).map (category) ->
            d = Data[category.id]
            if Editing
              for m in category.template
                arr(d, m)

            metrics = Object.keys(d).map(getMetric).sort(bySortId)
            if metrics.length > 1
              metrics.shift() # remove 'id' (necessarily first)
            si = Cols.length
            Cols.push.apply Cols, _.map metrics, (m) ->
              category: category
              metric: m
              sortable: m.id != 'id' || metrics.length == 1
            l = metrics.length
            Cols[si].first = Cols[si+l-1].last = l
            {
              category: category
              metrics: metrics
              start: si
            }
          $scope.cols = Cols
          if Editing
            ### jshint ignore:start #### fixed in jshint 2.5.7
            $scope.categories = (c for ci, c of constants.category when ci not of Data)
            ### jshint ignore:end ###
            $scope.categories.sort(bySortId)
            $scope.categories.push(pseudoCategory[0]) unless 0 of Data
          return

        # Call all populate functions
        populate = ->
          Data = {}
          Data.slot = {id:[]}
          Data.asset = {id:[]} if Assets
          Depends = {}
          for s, i in Slots
            populateSlot(i)
          populateCols()
          generate()
          return

        ################################# Generate HTML
        
        # Add or replace the text contents of cell c for measure/type m with value v
        generateText = (info) ->
          $(cell = info.cell).empty()
          v = info.v
          switch info.metric.id
            when 'name'
              if info.d
                a = cell.appendChild(document.createElement('a'))
                if info.c == 'asset'
                  icon = a.appendChild(document.createElement('img'))
                  icon.src = info.asset.icon
                  icon.className = "format hint-format-" + info.asset.format.extension
                  v ?= ''
                  t = {asset:info.d}
                else
                  stop = info.slot.id == Volume.top.id
                  if Editing && !stop
                    del = cell.insertBefore(document.createElement('a'), a)
                    del.className = 'trash icon'
                    i = info.i
                    $(del).on 'click', (event) ->
                      $scope.$apply () ->
                        removeSlot(cell, i)
                        return
                      event.stopPropagation()
                      return
                  a.className = "session icon hint-action-slot"
                  v ?= if stop then constants.message('materials.top') else ''
                  if stop then cell.classList.add('top-level-materials');
                  t = {}
                a.setAttribute('href', if Editing then info.slot.editRoute(t) else info.slot.route(t))
              else
                v ?= ''
            when 'release', 'classification'
              cn = constants.release[v || 0]
              cell.className = cn + ' release icon hint-release-' + cn
              v = ''
            when 'excerpt'
              if v
                cell.className = 'icon bullet'
              v = ''
            when 'id'
              if v?
                cell.className = 'icon ' + if Editing then 'trash' else 'bullet'
                v = ''
            when 'age'
              v = display.formatAge(v)
          if v?
            cell.classList.remove('blank')
          else
            cell.classList.add('blank')
            v = info.metric.assumed || ''
          cell.appendChild(document.createTextNode(v))
          return

        # Add a td element to tr r with value c and id i
        generateCell = (info) ->
          info.cell = info.row.appendChild(document.createElement('td'))
          if info.v == null
            info.cell.className = 'null'
          else
            generateText(info)
            info.cell.id = info.id
          return

        generateMultiple = (info) -> # (col, cols, row, i, n, t) ->
          t = info.count
          return if (if info.hasOwnProperty('n') then info.n < t else t == 1)
          td = info.row.appendChild(document.createElement('td'))
          width = info.cols.metrics.length
          td.setAttribute("colspan", width)
          if info.hasOwnProperty('n') || t <= 1
            td.className = 'null'
            if !info.n || info.n == t
              if Editing && width > 1
                td.appendChild(document.createTextNode("\u2190 add " + info.category.name))
              else if !info.n
                td.appendChild(document.createTextNode(info.category.not))
              if Editing
                info.metric = info.cols.metrics[0]
                if info.metric.id != 'id'
                  info.id = ID+'-rec_'+info.i+(if info.n? then '_'+info.n else '')+'_'+info.cols.start
                  info.v = undefined
                  info.d = undefined
                  generateCell(info)
                  if width > 1
                    info.row.appendChild(td)
                    td.setAttribute("colspan", width-1)
                  else
                    info.row.removeChild(td)
                  td.className = 'null'
                else
                  td.className = 'null add'
                  td.id = ID + '-add_' + info.i + '_' + info.c
          else
            td.appendChild(document.createTextNode(t + " " + info.category.name + "s"))
            td.className = 'more'
            td.id = ID + '-more_' + info.i + '_' + info.c
          td

        # Add all the measure tds to row i for count n, record r
        generateRecord = (info) -> # (row, i, col, n) ->
          ms = info.cols.metrics
          return unless l = ms.length
          t = info.count
          r = Data[info.c]
          if td = generateMultiple(info) # (col, l, row, i, n, t)
            unless info.hasOwnProperty('n')
              for n in [0..t-1] by 1
                td.classList.add('ss-rec_' + r.id[n][info.i])
            return
          b = ID + '-rec_' + info.i + '_'
          if info.hasOwnProperty('n')
            b += info.n + '_'
          for mi in [0..l-1] by 1
            info.metric = ms[mi]
            info.v = r[info.metric.id][info.n]?[info.i]
            info.d = r.id[info.n][info.i]
            info.id = b + (info.cols.start+mi)
            generateCell(info)
            if info.v != null
              info.cell.classList.add(ri = 'ss-rec_' + info.d)
              info.cell.classList.add(ri + '_' + info.metric.id)
          return

        # Fill out rows[i].
        generateRow = (i) ->
          info = new Info()
          info.i = i
          row = if Rows[i]
              $(Rows[i]).empty()
              Rows[i]
            else
              Rows[i] = document.createElement('tr')
          row.id = ID + '_' + i
          row.data = i
          if Editing && info.slot.id == Volume.top.id
            row.className = 'top'

          for col in Groups
            info.category = (info.cols = col).category
            generateRecord(info)
          return

        # Update all age displays.
        $scope.$on 'displayService-toggleAge', ->
          info = new Info()
          for m, mi in Cols
            continue unless m.metric.id == 'age'
            info.m = mi
            r = Data[info.c][info.metric.id]
            pre = ID + '-rec_'
            post = '_' + mi
            if expanded?.c == info.c && expanded.count > 1
              info.i = expanded.i
              for n in [0..info.count-1] by 1 when n of r
                info.n = n
                info.v = r[n][info.i]
                info.cell = document.getElementById(pre + info.i + '_' + n + post)
                generateText(info) if info.cell
            return unless 0 of r
            r = r[0]
            for d, i in r
              if Counts[i][info.c] == 1
                info.i = i
                info.v = d
                info.cell = document.getElementById(pre + i + post)
                generateText(info) if info.cell

        # Generate all rows.
        generate = ->
          for s, i in Slots
            generateRow(i)
          fill()
          return

        ################################# Place DOM elements
        
        # Place all rows into spreadsheet.
        fill = ->
          collapse()
          delete $scope.more
          for i, n in Order
            if n >= Limit
              $scope.more = Order.length
              TBody.removeChild(Rows[i]) if Rows[i].parentNode
            else
              TBody.appendChild(Rows[i])
          return

        # Populate order based on compare function applied to values.
        sort = (values, compare) ->
          return unless values
          compare ?= byMagic
          idx = new Array(Slots.length)
          for o, i in Order
            idx[o] = i
          Order.sort (i, j) ->
            compare(values[i], values[j]) || idx[i] - idx[j]
          return

        sort(Slots.map((s) -> s.date), byDefault)
        currentSort = 'date'
        currentSortDirection = false
  
        # Sort by values, called name.
        sortBy = (key, values) ->
          if currentSort == key
            currentSortDirection = !currentSortDirection
            Order.reverse()
          else
            sort(values)
            currentSort = key
            currentSortDirection = false
          fill()
          return

        # Sort by one of the container columns.
        sortBySlot = (f) ->
          sortBy(f, Slots.map((s) -> s[f]))

        # Sort by Category_id c's Metric_id m
        sortByMetric = (col) ->
          sortBy(col, Data[col.category.id][col.metric.id][0])

        $scope.colClasses = (col) ->
          cls = []
          if typeof col == 'object'
            cls.push 'first' if col.first
            cls.push 'last' if col.last
            cls.push 'sort' if col.sortable
          else
            cls.push 'sort'
          if currentSort == col
            cls.push 'sort-'+(if currentSortDirection then 'desc' else 'asc')
          else
            cls.push 'sortable'
          cls

        ################################# Backend saving

        setFocus = undefined

        saveRun = (cell, run) ->
          messages.clear(cell)
          cell.classList.remove('error')
          cell.classList.add('saving')
          run.then (res) ->
              cell.classList.remove('saving')
              res
            , (res) ->
              cell.classList.remove('saving')
              cell.classList.add('error')
              messages.addError
                body: 'Error saving data' # FIXME
                report: res
                owner: cell
              return

        createSlot = (cell) ->
          saveRun cell, Volume.createContainer({top:Top}).then (slot) ->
            arr(slot, 'records')
            i = Slots.push(slot)-1
            Order.push(i)
            populateSlot(i)
            generateRow(i)
            TBody.appendChild(Rows[i])
            return

        removeSlot = (cell, i) ->
          # assuming we have a container
          saveRun cell, Slots[i].remove().then (done) ->
            unless done
              messages.add
                body: constants.message('slot.remove.notempty')
                type: 'red'
                owner: cell
              return
            unedit(false)
            collapse()
            $(Rows[i]).remove()
            Slots.splice(i, 1)
            Counts.splice(i, 1)
            Rows.splice(i, 1)
            Order.remove(i)
            Order = Order.map (j) -> j - (j > i)
            populate()
            return

        setRecord = (info, record) ->
          add = ->
            if record
              info.slot.addRecord(record)
            else if record != null
              info.slot.newRecord(info.c || '')
          act =
            if info.record
              info.slot.removeRecord(info.record).then(add)
            else
              add()

          saveRun info.cell, act.then (record) ->
            o = info.d
            if record
              r = record.id
              info.n = inc(Counts[info.i], info.c) unless info.record

              for m, rcm of Data[info.c]
                v = if m of record then record[m] else record.measures[m]
                if v == undefined
                  delete rcm[info.n][info.i] if info.n of rcm
                else
                  arr(rcm, info.n)[info.i] = v
              # TODO this may necessitate regenerating column headers
            else
              t = --Counts[info.i][info.c]
              for m, rcm of Data[info.c]
                for n in [info.n+1..rcm.length-1] by 1
                  arr(rcm, n-1)[info.i] = arr(rcm, n)[info.i]
                delete rcm[t][info.i] if t of rcm

            delete Depends[o][info.i] if o?
            obj(Depends, r)[info.i] = info.n if record

            collapse()
            generateRow(info.i)
            expand(info) if info.n
            if record && setFocus == (i = info.id) && (i = document.getElementById(i)?.nextSibling) && (i = parseId(i))
              select(i)
            setFocus = undefined
            record

        saveDatum = (info, v) ->
          if info.c == 'slot'
            data = {}
            data[info.metric.id] = v ? ''
            return if info.slot[info.metric.id] == v
            saveRun info.cell, info.slot.save(data).then () ->
              info.v = Data[info.c][info.metric.id][info.n][info.i] = v
              generateText(info) #, cell, info.t, info.slot[info.t])
              return
          else if info.c == 'asset'
            data = {}
            t = info.metric.id
            data[t] = v ? ''
            return if info.asset[t] == data[t]
            saveRun info.cell, info.asset.save(data).then () ->
              info.v = Data[info.c][info.metric.id][info.n][info.i] = v
              generateText(info)
              return
          else
            return if info.record.measures[info.metric.id] == v
            saveRun info.cell, info.record.measureSet(info.metric.id, v).then (rec) ->
              rcm = Data[rec.category || 0][info.metric.id]
              for i, n of Depends[info.d]
                arr(rcm, n)[i] = v
                # TODO age may have changed... not clear how to update.
              info.v = v
              l = TBody.getElementsByClassName('ss-rec_' + info.d + '_' + info.metric.id)
              for li in l
                info.cell = li
                generateText(info)
              return

        ################################# Interaction

        expanded = undefined # Info

        # Collapse any expanded row.
        collapse = ->
          return unless expanded
          i = expanded.i
          expanded = undefined
          row = Rows[i]
          row.classList.remove('expand')
          t = 0
          while (el = row.nextSibling) && el.data == i
            t++
            $(el).remove()

          el = row.firstChild
          while el
            el.removeAttribute("rowspan")
            el = el.nextSibling

          t

        # Expand (or collapse) a row
        expand = (info) ->
          if expanded?.i == info.i && `expanded.c == info.c`
            if info.t == 'more'
              collapse()
            return
          collapse()

          expanded = new Info()
          expanded.i = info.i
          expanded.c = info.c
          info.row.classList.add('expand')

          max = expanded.count
          max++ if Editing && !expanded.category.fixed
          return if max <= 1
          next = info.row.nextSibling
          start = expanded.count == 1
          for n in [+start..max-1] by 1
            expanded.n = n
            expanded.row = TBody.insertBefore(document.createElement('tr'), next)
            expanded.row.data = expanded.i
            expanded.row.className = 'expand'
            generateRecord(expanded)
          expanded.row = info.row

          max++ unless start
          el = info.row.firstChild
          while el
            info = new Info(el)
            if `info.c != expanded.c`
              el.setAttribute("rowspan", max)
            el = el.nextSibling
          return

        save = (info, type, value) ->
          if value == ''
            value = undefined
          else switch type
            when 'release'
              value = parseInt(value, 10)
            when 'record'
              if value == 'new'
                setRecord(info)
              else if value == 'remove'
                setRecord(info, null) if info.d?
              else if v = stripPrefix(value, 'add_')
                u = v.indexOf('_')
                info.metric = constants.metric[v.slice(0,u)]
                v = v.slice(u+1)
                setRecord(info).then (r) ->
                  info.record = r
                  saveDatum(info, v) if r
                  return
              else if !isNaN(v = parseInt(value, 10))
                if v != info.d
                  setRecord(info, Volume.records[v])
              return
            when 'metric'
              if value != undefined
                arr(Data[info.c], value)
                populateCols()
                generate()
              return
            when 'category'
              if value != undefined
                arr(obj(Data, value), 'id')
                populateCols()
                generate()
              return
            when 'options'
              # force completion of the first match
              # this completely prevents people from using prefixes of options but maybe that's reasonable
              c = optionCompletions(value) if value
              value = c[0] if c?.length

          if type == 'ident'
            r = editScope.identCompleter(value)
            r.find((o) -> o.default)?.run(info) if Array.isArray(r)
            return

          saveDatum(info, value)

        editScope = $scope.$new(true)
        editScope.constants = constants
        editInput = editScope.input = {}
        editCellTemplate = $compile($templateCache.get('volume/spreadsheetEditCell.html'))
        editCell = undefined

        unedit = (event) ->
          return unless edit = editCell
          editCell = undefined
          cell = edit.parentNode
          $(edit).remove()
          return unless cell?.parentNode
          cell.classList.remove('editing')
          tooltips.clear()

          info = new Info(cell)
          save(info, editScope.type, editInput.value) if event != false
          info

        recordDescription = (r) ->
          k = Object.keys(r.measures)
          if k.length
            k.sort(byNumber).map((m) -> r.measures[m]).join(', ')
          else
            '[' + r.id + ']'

        edit = (info) ->
          switch info.t
            when 'rec'
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute({asset:info.d}))
                return
              return if info.slot.id == Volume.top.id
              m = info.metric.id
              if m == 'id'
                # trash/bullet: remove
                setRecord(info, null)
                return
              return if info.metric.readonly
              editScope.type = info.metric.type
              if info.c == 'slot'
                v = info.slot[m]
                if m == 'release'
                  ### jshint ignore:start ###
                  v ||= 0
                  ### jshint ignore:end ###
              else if info.c == 'asset' # not reached
                v = info.asset[m]
              else
                v = info.record?.measures[m]
                if info.col.first
                  editScope.type = 'ident'
                  editScope.info = info
                  rs = []
                  mf = (r) -> (m) -> r.measures[m]
                  for ri, r of Volume.records
                    if (r.category || 0) == info.category.id && !(ri of Depends && info.i of Depends[ri])
                      rs.push
                        r:r
                        v:(r.measures[info.metric.id] ? '').toLowerCase()
                        d:recordDescription(r)
                  editScope.records = rs.sort((a, b) -> byMagic(a.v, b.v))
                else if info.metric.options
                  editScope.type = 'options'
                  editScope.options = info.metric.options
                else if info.metric.long
                  editScope.type = 'long'
              editInput.value = (v ? '')+''
            when 'add'
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute())
                return
              return if info.slot.id == Volume.top.id
              c = info.category
              if info.d?
                editInput.value = info.d + ''
              else
                editInput.value = 'remove'
              editScope.type = 'record'
              editScope.options =
                new: 'Create new ' + c.name
                remove: c.not
              for ri, r of Volume.records
                if (r.category || 0) == c.id && (!(ri of Depends && info.i of Depends[ri]) || ri == editInput.value)
                  editScope.options[ri] = r.displayName
              # detect special cases: singleton or unitary records
              for mi of Data[c.id]
                mm = constants.metric[mi]
                if !m
                  m = mm
                else if mm
                  m = null
                  break
              if m == undefined && Object.keys(editScope.options).length > 2
                # singleton: id only, existing record(s)
                delete editScope.options['new']
              else if m && m.options
                # unitary: single metric with options
                delete editScope.options['new']
                for o in m.options
                  found = false
                  for ri, r of Volume.records
                    if (r.category || 0) == c.id && r.measures[m.id] == o
                      found = true
                      break
                  editScope.options['add_'+m.id+'_'+o] = o unless found
            when 'category'
              editScope.type = 'metric'
              editInput.value = undefined
              editScope.options = []
              for mi, m of constants.metric when !(mi of Data[info.c])
                editScope.options.push(m)
              editScope.options.sort(bySortId)
            when 'head'
              editScope.type = 'category'
              editInput.value = undefined
              editScope.options = $scope.categories
            else
              return

          e = editCellTemplate editScope, (e) ->
            info.cell.insertBefore(editCell = e[0], info.cell.firstChild)
            info.cell.classList.add('editing')
            return
          e.on 'click', ($event) ->
            # prevent other ng-click handlers from taking over
            $event.stopPropagation()
            return

          tooltips.clear()
          $timeout ->
            input = e.find('[name=edit]')
            input.filter('input,textarea').focus().select()
            input.filter('select').focus().one('change', $scope.$lift(editScope.unedit))
            return
          return

        unselect = ->
          styles.clear()
          unedit()
          return

        $scope.$on '$destroy', unselect

        select = (info) ->
          unselect()
          expand(info)
          if info.t == 'rec'
            for c in info.cell.classList when c.startsWith('ss-rec_')
              styles.set('.' + c + '{background-color:' +
                (if c.includes('_', 7) then 'rgba(226,217,0,0.6)' else 'rgba(242,238,100,0.4)') +
                ';\n text-}')

          edit(info) if Editing
          return

        $scope.click = (event) ->
          el = event.target
          return unless el.tagName == 'TD' && info = parseId(el)

          select(info)
          if info.metric?.id == 'age'
            display.toggleAge()
          return

        doneEdit = (event, info) ->
          if info && event && event.$key == 'Tab'
            setFocus = !event.shiftKey && info.cell.id
            c = info.cell
            while true
              c = if event.shiftKey then c.previousSibling else c.nextSibling
              return unless c && c.tagName == 'TD' && i = parseId(c)
              break unless info.t == 'rec' && info.metric.id == 'id' # skip "delete" actions
            select(i)

          return

        editScope.unedit = (event) ->
          doneEdit(event, unedit(event))
          false

        editSelect = (event) ->
          editInput.value = @text
          editScope.unedit(event)
          @text

        editScope.identCompleter = (input) ->
          info = editScope.info
          o = []
          defd = false
          add = (t, f, d) ->
            o.push
              text: t
              select: (event) ->
                info = unedit(false)
                f(info)
                doneEdit(event, info)
                return
              run: f
              default: d && !defd
            defd ||= d
          if info.d?
            if input == info.record.measures[info.metric.id]
              add("Keep " + info.record.displayName,
                () -> return,
                true)
            if !input
              add("Remove " + info.record.displayName + " from this session",
                (info) -> setRecord(info, null),
                true)
          if !info.d? || input && input != info.record.measures[info.metric.id]
            inputl = (input ? '').toLowerCase()
            set = (r) -> (info) ->
              setRecord(info, r)
            rs = (r for r in editScope.records when r.v.startsWith(inputl))
            for r in rs
              add("Use " + info.category.name + ' ' + r.d, set(r.r), input && rs.length == 1 || r.v == inputl)
            os = if info.metric.options
                (x for x in info.metric.options when x.toLowerCase().startsWith(inputl))
              else
                []
            if input && !os.length
              os = [input]
            os.forEach (i) ->
              if info.d?
                add("Change all " + info.record.displayName + " " + info.metric.name + " to '" + i + "'",
                  (info) -> saveDatum(info, i),
                  input && !rs.length && os.length == 1 || i == input)
              add("Create new " + info.category.name + " with " + info.metric.name + " '" + i + "'",
                (info) -> setRecord(info).then((r) ->
                  info.record = r
                  saveDatum(info, i) if r
                  return),
                input && !rs.length && os.length == 1 || i == input)
          if o.length then o else input

        optionCompletions = (input) ->
          i = input.toLowerCase()
          (o for o in editScope.options when o.toLowerCase().startsWith(i))

        editScope.optionsCompleter = (input) ->
          match = optionCompletions(input)
          switch match.length
            when 0
              input
            when 1
              match[0]
            else
              ({text:o, select: editSelect, default: input && i==0} for o, i in match)

        $scope.clickAdd = ($event) ->
          unselect()
          edit({cell:$event.target, t:'head'})
          return

        $scope.clickCategoryAdd = ($event, col) ->
          unselect()
          edit({cell:$event.target.parentNode, t:'category', c:col.category.id}) if Editing
          return

        $scope.clickMetric = (col) ->
          sortByMetric(col) if col.sortable
          return

        $scope.clickNew = ($event) ->
          createSlot($event.target)
          return

        $scope.unlimit = ->
          Limit = undefined
          fill()

        if Editing
          $document.on 'click', ($event) ->
            if editCell && editCell.parentNode != $event.target && !$.contains(editCell.parentNode, $event.target)
              $scope.$applyAsync(unedit)
            return

        ################################# main

        $scope.refresh = ->
          unedit()
          collapse()
          populate()
          return

        populate()
        return
    ]
    }
]
