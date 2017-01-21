Ext.onReady(function(){

    var store = new Ext.data.GroupingStore({
        proxy: new Ext.data.HttpProxy({url: 'queues'}),
        reader: new Ext.data.JsonReader({root: "queuerows",id: "id"}, [
            "state",
            "try",
            "status",
            "statustime",
            "resultsip",
            "resultsms",
            "resultmail",
            "id",
            "phone",
            "mobilephone",
            "homephone",
            "organisationname",
            "divisionname",
            "personname",
            "positionname",
            "site",
            "group",
            "email",
            "code",
            "ringgroupname",
            "organisation",
            "division",
            "address1",
            "address2"
        ]),
        sortInfo:{field: 'id', direction: "ASC"},
        groupField:'state'
    });

    var ringlist = new Ext.data.GroupingStore({
        proxy: new Ext.data.HttpProxy({url: 'ringlist'}),
        reader: new Ext.data.JsonReader({root: "ringlist",id: "id"}, [
            "state",
            "id",
            "phone",
            "mobilephone",
            "homephone",
            "organisationname",
            "divisionname",
            "personname",
            "positionname",
            "site",
            "group",
            "email",
            "code",
            "ringgroupname",
            "organisation",
            "division",
            "address1",
            "address2"
        ]),
        sortInfo:{field: 'id', direction: "ASC"},
        groupField:'state'
    });

    var report = new Ext.data.GroupingStore({
        proxy: new Ext.data.HttpProxy({url: 'report'}),
        reader: new Ext.data.JsonReader({root: "status",id: "id"}, [
            "state",
            "try",
            "status",
            "statustime",
            "resultsip",
            "resultsms",
            "resultmail",
            "id",
            "phone",
            "mobilephone",
            "homephone",
            "organisationname",
            "divisionname",
            "personname",
            "positionname",
            "site",
            "group",
            "email",
            "code",
            "ringgroupname",
            "organisation",
            "division",
            "address1",
            "address2",
            "journal"
        ]),
        sortInfo:{field: 'id', direction: "ASC"},
        groupField:'state'
    });

    var queues = new Ext.data.JsonStore({
        root: "queues",
        id: "queueid",
        fields:["queueid","name","time","count"]
    });

    var chanlo = new Ext.data.JsonStore({
        root: "chanlo",
        id: "channel",
        fields:["channel","status","event","script","action"]
    });

    var chanhi = new Ext.data.JsonStore({
        root: "chanhi",
        id: "channel",
        fields:["channel","status","event","script","action"]
    });

    var eventlist = new Ext.data.JsonStore({
        root: "events",
        id: "eventid",
        url: "events",
        fields:["eventid","eventname"]
    });

    eventlist.on('load',function(){
        eventlist.mode=eventlist.reader.jsonData.mode;
    });

    var reportlist = new Ext.data.JsonStore({
        root: "reports",
        id: "fileid",
        url: "reports",
        fields:["fileid","filename"]
    });

    reportlist.on('load',function(){
        reportlist.mode=reportlist.reader.jsonData.mode;
    });
    
    function renderResult(data, cell, record, rowIndex, columnIndex, store){
                if (data)
                  cell.css = 'result'+data;
                return;
            };

    var grid = new Ext.grid.GridPanel({
        border: false,
        region: 'center',
        store: store,
        columns: [
            {header: "състояние", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "опит", width: 40, sortable: true, dataIndex: "try"},
            {header: "статус", width: 160, sortable: true, dataIndex: "status"},
            {header: "дата и час", width: 120, sortable: true, dataIndex: "statustime"},
            {header: "PRI", width: 35, sortable: true, dataIndex: "resultsip", renderer: renderResult},
            {header: "SMS", width: 35, sortable: true, dataIndex: "resultsms", renderer: renderResult},
            {header: "Mail", width: 35, sortable: true, dataIndex: "resultmail", renderer: renderResult},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text} ({[values.rs.length]})'
        }),
        listeners:{
            rowclick:function(grid,rowindex){
                    var data=grid.selModel.getSelected().data;
                    detailstemplate.overwrite(Ext.getCmp('ringdetails').body,data);
            }
        }

    });

    var detailstemplate=new Ext.XTemplate.from("detailstemplate");
    
    var ringgrid = new Ext.grid.GridPanel({
        border: false,
        store: ringlist,
        columns: [
            {header: "сценарий", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "служебен телефон", width: 120, sortable: true, dataIndex: "phone"},
            {header: "мобилен телефон", width: 120, sortable: true, dataIndex: "mobilephone"},
            {header: "домашен телефон", width: 120, sortable: true, dataIndex: "homephone"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "име", width: 120, sortable: true, dataIndex: "personname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "email", width: 120, sortable: true, dataIndex: "email"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"},
            {header: "сл.адрес", width: 360, sortable: true, dataIndex: "address1"},
            {header: "дом.адрес", width: 360, sortable: true, dataIndex: "address2"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text} ({[values.rs.length]})'
        })
    });

    var reportstatus = new Ext.grid.GridPanel({
        border: false,
        region:'center',
        store: report,
        columns: [
            {header: "резултат", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "опит", width: 40, sortable: true, dataIndex: "try"},
            {header: "статус", width: 120, sortable: true, dataIndex: "status"},
            {header: "дата и час", width: 120, sortable: true, dataIndex: "statustime"},
            {header: "PRI", width: 35, sortable: true, dataIndex: "resultsip", renderer: renderResult},
            {header: "SMS", width: 35, sortable: true, dataIndex: "resultsms", renderer: renderResult},
            {header: "Mail", width: 35, sortable: true, dataIndex: "resultmail", renderer: renderResult},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text} ({[values.rs.length]})'
        }),
        listeners:{
            rowclick:function(grid,rowindex){
                    var data=grid.selModel.getSelected().data;
                    journal2.setRawValue(data.journal);
                    detailstemplate.overwrite(Ext.getCmp('reportdetails').body,data);
            }
        }
        
    });

    var queuegrid = new Ext.grid.GridPanel({
        border: false,
        store: queues,
        columns: [
            {header: "id", width: 30, sortable: true, dataIndex: "queueid", hidden:"true"},
            {header: "име", width: 120, sortable: true, dataIndex: "name"},
            {header: "време", width: 90, sortable: true, dataIndex: "time"},
            {header: "обекти", width: 50, sortable: true, dataIndex: "count"}
        ]

    });

    var eventgrid = new Ext.grid.GridPanel({
        border: false,
        store: eventlist,
        columns: [
            {header: "id", width: 30, sortable: true, dataIndex: "eventid", hidden:"true"},
            {header: "име", width: 150, sortable: true, dataIndex: "eventname"}
        ],
        listeners:{
            rowclick:function(grid,rowindex){
                ringlist.removeAll();
                ringlist.load({
                    params:{mode:eventlist.mode,
                            eventid:grid.selModel.hasSelection()?grid.selModel.getSelected().id:0
                    }
                });
            }
        }
    });

    var reportgrid = new Ext.grid.GridPanel({
        border: false,
        store: reportlist,
        columns: [
            {header: "id", width: 30, sortable: true, dataIndex: "fileid", hidden:"true"},
            {header: "име", width: 250, sortable: true, dataIndex: "filename"}
        ],
        listeners:{
            rowclick:function(grid,rowindex){
                journal1.setRawValue('');
                journal2.setRawValue('');
                Ext.getCmp('reportdetails').body.update('');
                report.removeAll();
                report.load({
                    params:{mode:reportlist.mode,
                            fileid:grid.selModel.hasSelection()?grid.selModel.getSelected().id:0
                    }
                });
            }
        }
    });

        fields:["channel","status","event","script","action"]

	var gridlo = new Ext.grid.GridPanel({
        border: false,
        store: chanlo,
        columnWidth: 0.5,
        autoHeight:true,
        stripeRows:true,
        columns: [
            {header: "Канал", width: 50, sortable: true, dataIndex: "channel"},
            {header: "Събитие", width: 130, sortable: true, dataIndex: "event"},
            {header: "Сценарий", width: 130, sortable: true, dataIndex: "script"},
            {header: "Стъпка", width: 50, sortable: true, dataIndex: "action"},
            {header: "Статус", width: 260, sortable: true, dataIndex: "status"}
        ]
    });

	var gridhi = new Ext.grid.GridPanel({
        border: false,
        store: chanhi,
        autoHeight:true,
        stripeRows:true,
	columnWidth: 0.5,
        columns: [
            {header: "Канал", width: 50, sortable: true, dataIndex: "channel"},
            {header: "Събитие", width: 130, sortable: true, dataIndex: "event"},
            {header: "Сценарий", width: 130, sortable: true, dataIndex: "script"},
            {header: "Стъпка", width: 50, sortable: true, dataIndex: "action"},
            {header: "Статус", width: 260, sortable: true, dataIndex: "status"}
        ]
    });

    var journal1 = new Ext.form.TextArea({
        border:false
    });

    var journal2 = new Ext.form.TextArea({
        border:false
    });

    var monitor_gsm = new Ext.Toolbar.TextItem("");
    var monitor_rs = new Ext.Toolbar.TextItem("");
    var monitor_sip = new Ext.Toolbar.TextItem("");
    var monitor_mail = new Ext.Toolbar.TextItem("");
    var monitor_mode = new Ext.Toolbar.TextItem("");

    report.on('load',function(){
        journal1.setRawValue(report.reader.jsonData.log);
    });

    var modewindow = new Ext.Window({
        title:"Промяна на режима",
        layout:"fit",
        closeAction:"hide",
        modal: true,
        applyTo:"modewindow",
        width:400,
        height:260,
        items:[
            new Ext.FormPanel({
                id: "modepanel",
                bodyStyle:"padding:5px 5px 0",
                labelWidth:200,
                url:"mode",
                items:[
                  {xtype: "radio", name: "mode", inputValue: 1, fieldLabel: "Тренировъчен режим"},
                  {xtype: "radio", name: "mode", inputValue: 2, fieldLabel: "Дежурен режим"},
                  {xtype: "radio", name: "mode", inputValue: 3, fieldLabel: "Преход военновременен режим"},
                  {xtype: "radio", name: "mode", inputValue: 4, fieldLabel: "Военновременен режим"},
                  {xtype: "textfield", id:"flduser", fieldLabel: "Потребителско име", name: "username", msgTarget: "title"},
                  {xtype: "textfield", id:"fldpass", fieldLabel: "Парола", name: "password", inputType: "password", msgTarget: "title"}
               ],
               buttons: [{
                    text     : "Промяна",
                    handler  : function(){
                        Ext.ComponentMgr.get("modepanel").getForm().submit({
                           method:"put",
                           success:function(form,action){
                             modewindow.hide();
                           },
                           failure:function(form,action){
                             ;
                           }
                        });
                    }
                },{
                    text     : "Отказ",
                    handler  : function(){
                        modewindow.hide();
                    }
                }]
            })
        ],
        listeners:{
            "beforeshow":{fn:function(){
                Ext.ComponentMgr.get("flduser").reset();
                Ext.ComponentMgr.get("fldpass").reset();
            }}
        }
    });

    var changemode = function(){

       Ext.ComponentMgr.get("modepanel").getForm().load({
          method:"get",
          success:function(){modewindow.show()},
          failure:function(){alert('Грешка при връзка със сървъра');}
       });
    }

    var events = new Ext.data.JsonStore({
        root: "events",
        id: "eventid",
        url: "events",
        fields:["eventid","eventname"]
    });

    events.on('load',function(){
        events.mode=events.reader.jsonData.mode;
    });


    var eventwindow = new Ext.Window({
        title:"Избор на събитие за оповестяване",
        layout:"anchor",
        closeAction:"hide",
        modal: true,
        applyTo:"eventwindow",
        width:400,
        height:600,
        items:[
            new Ext.grid.GridPanel({
                    anchor: "100% -100",
                    id:"eventgrid",
                    border: false,
                    store: events,
                    columns: [
                        {header: "id", width: 30, sortable: true, dataIndex: "eventid", hidden:"true"},
                        {header: "Събитие", width: 350, sortable: true, dataIndex: "eventname", menuDisabled: true}
                    ]

                }),
            new Ext.Panel({
                    height:2
                }),
            new Ext.FormPanel({
                bodyStyle:"padding:5px 5px 0",
                border: false,
                id:"eventpanel",
                url:"fireevent",
                labelWidth:200,
                items:[
                  {xtype: "textfield", id:"eventuser", fieldLabel: "Потребителско име", name: "username", msgTarget: "title"},
                  {xtype: "textfield", id:"eventpass", fieldLabel: "Парола", name: "password", inputType: "password", msgTarget: "title"}
               ],
               buttons: [{
                    text     : "Стартиране",
                    handler  : function(){
                        var g=Ext.ComponentMgr.get("eventgrid");
                        var key=(g && g.selModel && g.selModel.hasSelection())?g.selModel.getSelected().id:null;
                        if (key){
                            Ext.ComponentMgr.get("eventpanel").getForm().submit({
                               params:{eventid:key,mode:events.mode},
                               success:function(form,action){
                                 eventwindow.hide();
                               },
                               failure:function(form,action){
                                 ;
                               }
                            });
                        }
                    }
                },{
                    text     : "Отказ",
                    handler  : function(){
                        eventwindow.hide();
                    }
                }]
            })


        ],
        listeners:{
            "beforeshow":{fn:function(){
                Ext.ComponentMgr.get("eventuser").reset();
                Ext.ComponentMgr.get("eventpass").reset();
            }}
        }
    });

    var startevent = function(){
        events.removeAll();
        events.load();
        eventwindow.show();
    }


    var eventstopwindow = new Ext.Window({
        eventid:0,
        title:"Спиране на оповестяване",
        layout:"anchor",
        closeAction:"hide",
        modal: true,
        applyTo:"eventstopwindow",
        width:400,
        height:200,
        items:[
            new Ext.FormPanel({
                bodyStyle:"padding:5px 5px 0",
                border: false,
                id:"eventstoppanel",
                url:"stopevent",
                labelWidth:200,
                items:[
                  {xtype: "textfield", id:"eventstopuser", fieldLabel: "Потребителско име", name: "username", msgTarget: "title"},
                  {xtype: "textfield", id:"eventstoppass", fieldLabel: "Парола", name: "password", inputType: "password", msgTarget: "title"}
               ],
               buttons: [{
                    text     : "Спиране",
                    handler  : function(){
                        Ext.ComponentMgr.get("eventstoppanel").getForm().submit({
                           params:{eventid:eventstopwindow.eventid},
                           success:function(form,action){
                             eventstopwindow.hide();
                           },
                           failure:function(form,action){
                             ;
                           }
                        });
                    }
                },{
                    text     : "Отказ",
                    handler  : function(){
                        eventstopwindow.hide();
                    }
                }]
            })


        ],
        listeners:{
            "beforeshow":{fn:function(){
                Ext.ComponentMgr.get("eventstopuser").reset();
                Ext.ComponentMgr.get("eventstoppass").reset();
            }}
        }
    });

    var stopevent = function(){
        eventstopwindow.eventid=queuegrid.selModel.hasSelection()?queuegrid.selModel.getSelected().id:0;
        if (eventstopwindow.eventid>0){
            eventstopwindow.show();
        }
    }

    var stop_event = new Ext.Toolbar.Button({text:"Спиране на оповестяване",handler:stopevent,cls:"x-btn-over"});
	
	var printreport = function(){
		var w=window.open();
		var tpl=new Ext.XTemplate.from("reporttemplate");
        var decode={fail:'неуспешно',succeed:'успешно',succeedhome:'домашен',succeedmobile:'мобилен',succeedwork:'служебен'};
        var data=report.reader.jsonData;
        Ext.each(data.status,function(x){
            if (x.resultsip) x.resultsipbg=decode[x.resultsip];
            if (x.resultsms) x.resultsmsbg=decode[x.resultsms];
            if (x.resultmail) x.resultmailbg=decode[x.resultmail];
        });    
		tpl.overwrite(w.document.body, data);		
	}

    var tabs = new Ext.TabPanel({
        plain:true,
        border:false,
        activeTab:0,
        items:[
            new Ext.Panel({
                id: 'tabevents',
                listeners: {activate: function(){
                        ringlist.removeAll();
                        eventlist.removeAll();
                        eventlist.load();
                    }},
                title:'Събития',
                layout:'border',
                items:[{ 
                    title:'събития',
                    region:'west',
                    margins: '5 0 5 5',
                    cmargins: '5 5 0 5',
                    width: 180,
                    minSize: 100,
                    maxSize: 300,
                    split: true,
                    layout: "fit",
                    items:eventgrid
                },{
                    title:'обекти за оповестяване',
                    region:'center',
                    margins: '5 5 5 0',
                    layout: "fit",
                    items:ringgrid
                }]
                })
        ,
            new Ext.Panel({
                id:'tabmonitoring',
                title:'Мониторинг',
                layout:'border',
                items:[{
                    region: 'south',
                    title:'канали',
                    height: 370,
                    minSize: 75,
                    margins: '0 5 5 5',
                    split: true,
                    layout: "column",
                    items:[
                        gridlo,
                        gridhi
                    ]
                },{ 
                    title:'активни оповестявания',
                    region:'west',
                    margins: '5 0 0 5',
                    cmargins: '5 5 0 5',
                    width: 290,
                    minSize: 100,
                    maxSize: 300,
                    split: true,
                    collapsible: true,
                    layout: "fit",
                    items:[queuegrid]
                },{
                    title:'обекти',
                    region:'center',
                    margins: '5 5 0 0',
                    layout: "border",
                    items:[grid,{title:'детайли',region:'south',height:60,id:'ringdetails'}]
                }]
                })
        ,
            new Ext.Panel({
                id: 'tabreports',
                listeners: {activate: function(){
                        report.removeAll();
                        reportlist.removeAll();
                        reportlist.load();
                        journal1.setRawValue('');
                        journal2.setRawValue('');
                    }},
                title:'Справки',
                layout:'border',
                items:[
                    
                    {
                        region: 'south',
                        height: 300,
                        minSize: 75,
                        margins: '0 5 5 5',
                        split: true,
                        layout: "border",
                        border:false,
                        items: [{
                            title:"журнал събитие", 
                            width:"50%",
                            region:"west",
                            layout:"fit",
                            split:true,
                            items:journal1
                            },{
                            title:"журнал обект",
                            region:"center",
                            layout:"fit",
                            items:journal2
                            }]
                        
                    }
                    
                ,{ 
                    title:'завършени оповестявания',
                    region:'west',
                    margins: '5 0 0 5',
                    cmargins: '5 5 0 5',
                    width: 270,
                    minSize: 100,
                    maxSize: 300,
                    split: true,
                    layout: "fit",
                    items: reportgrid
                },{
                    title:'резултат',
                    region:'center',
                    margins: '5 5 0 0',
                    layout: "border",
                    items: [reportstatus,{title:'детайли',region:'south',height:60,id:'reportdetails'}],
                    tools:[{id:'print',handler:printreport}
                    ]
                }]

                })

        ]

    });

    var border = new Ext.Viewport({
        title: 'Border Layout',
        layout:'border',
        items: [{
            region: 'north',
            height: 26,
            border: false,
            items: new Ext.Toolbar([
                {text:"Промяна на режима",handler:changemode,ctCls:"x-btn-over"},
                ' ',
                {text:"Стартиране на оповестяване",handler:startevent,ctCls:"x-btn-over"},
                ' ',
                stop_event,
                {xtype:"tbfill"},
                monitor_mode,
                monitor_gsm,
                monitor_rs,
                monitor_sip,
                monitor_mail
            ])
        },{
            region:'center',
            layout:'fit',
            border:false,
            items:[tabs]
        }
        ]
    });


    var cleartask= new Ext.util.DelayedTask(function(){
        store.removeAll();
        queues.removeAll();
        chanlo.removeAll();
        chanhi.removeAll();
        cleartask.cancel();
        stop_event.disable();
        monitor_mode.getEl().innerHTML="<span style='color:red;font-weight:bold'>Режим?</span>";
        monitor_rs.getEl().innerHTML="<span style='color:red;font-weight:bold'>RS?</span>";
        monitor_gsm.getEl().innerHTML="<span style='color:red;font-weight:bold'>GSM?</span>";
        monitor_mail.getEl().innerHTML="<span style='color:red;font-weight:bold'>Mail?</span>";
        monitor_sip.getEl().innerHTML="<span style='color:red;font-weight:bold'>PRI?</span>";
    });
    
    Ext.TaskMgr.start({
        run:function(){
            store.load({
                    callback:function(r,options,success){
                        if (success) cleartask.delay(3000);
                        if (store.scrollstate) {
                            grid.view.restoreScroll(store.scrollstate);
                        }
                        if (grid.selModel.getSelected()){
                            detailstemplate.overwrite(Ext.getCmp('ringdetails').body,grid.selModel.getSelected().data);
                        } else
                            Ext.getCmp('ringdetails').body.update('');
                    },
                    params:{
                        queueid:(tabs.activeTab.id=='tabmonitoring' && queuegrid.selModel.hasSelection())?queuegrid.selModel.getSelected().id:0
                    }
                });
        },
        interval:1000
    });

    store.on('load',function(){
        var key=null;
        var isactive = (tabs.activeTab.id=='tabmonitoring');

        if (isactive){
            key=queuegrid.selModel.hasSelection()?queuegrid.selModel.getSelected().id:null;
        }
        
        queues.loadData(store.reader.jsonData);
        
        if (isactive){
            //textarea.setRawValue(store.reader.jsonData.channels);
                        chanlo.loadData(store.reader.jsonData.channels);
                        chanhi.loadData(store.reader.jsonData.channels);
        }

        if (store.reader.jsonData.mode){
          mode=store.reader.jsonData.mode;
          if (mode==1)
            monitor_mode.getEl().innerHTML="<span style='color:green;font-weight:bold'>Тренировъчен режим</span>";
          else if (mode==2)
            monitor_mode.getEl().innerHTML="<span style='color:green;font-weight:bold'>Дежурен режим</span>";
          else if (mode==3)
            monitor_mode.getEl().innerHTML="<span style='color:green;font-weight:bold'>Преход военновременен режим</span>";
          else if (mode==4)
            monitor_mode.getEl().innerHTML="<span style='color:green;font-weight:bold'>Военновременен режим</span>";
        } else
          monitor_mode.getEl().innerHTML="<span style='color:red;font-weight:bold'>Режим?</span>";
        
        if (store.reader.jsonData.monitor.rs)
          monitor_rs.getEl().innerHTML="<span style='color:green;font-weight:bold'>RS</span>";
        else
          monitor_rs.getEl().innerHTML="<span style='color:red;font-weight:bold'>RS</span>";

        var gsm;
        var gsmreg;
        
        if (store.reader.jsonData.monitor.gsm)
          gsm="<span style='color:green;font-weight:bold'>GSM("+store.reader.jsonData.monitor.gsmcount+")</span>";
        else
          gsm="<span style='color:red;font-weight:bold'>GSM("+store.reader.jsonData.monitor.gsmcount+")</span>";

        if (store.reader.jsonData.monitor.gsmreg)
          gsmreg="<span style='color:green;font-weight:bold'>"+store.reader.jsonData.monitor.gsmreg+"</span>";
        else
          gsmreg="<span style='color:red;font-weight:bold'>no network</span>";
        

        monitor_gsm.getEl().innerHTML=gsm+'&nbsp;'+gsmreg;
        
        if (store.reader.jsonData.monitor.sip)
          monitor_sip.getEl().innerHTML="<span style='color:green;font-weight:bold'>PRI</span>";
        else
          monitor_sip.getEl().innerHTML="<span style='color:red;font-weight:bold'>PRI</span>";

        if (store.reader.jsonData.monitor.mail)
          monitor_mail.getEl().innerHTML="<span style='color:green;font-weight:bold'>Mail("+store.reader.jsonData.monitor.mailcount+")</span>";
        else
          monitor_mail.getEl().innerHTML="<span style='color:red;font-weight:bold'>Mail("+store.reader.jsonData.monitor.mailcount+")</span>";

        if (key) {
          queuegrid.selModel.selectRow(store.find("queueid",key));
          stop_event.enable();
        }
        else{
          stop_event.disable();
        }
    });

    store.on('beforeload',function(){
        if (tabs.activeTab.id=='tabmonitoring'){
            store.selectedkey=(grid.selModel.hasSelection())?grid.selModel.getSelected().id:null;
            store.scrollstate=grid.view.getScrollState();
        } else
        {   store.scrollstate=null;
        }
    });

});