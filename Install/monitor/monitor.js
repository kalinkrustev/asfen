Ext.onReady(function(){

    var store = new Ext.data.GroupingStore({
        proxy: new Ext.data.HttpProxy({url: 'queues'}),
        reader: new Ext.data.JsonReader({root: "queuerows",id: "id"}, [
            "state",
            "status",
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
            "status",
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

    var queues = new Ext.data.JsonStore({
        root: "queues",
        id: "queueid",
        fields:["queueid","name","time"]
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

    var grid = new Ext.grid.GridPanel({
        border: false,
        store: store,
        columns: [
            {header: "състояние", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "статус", width: 120, sortable: true, dataIndex: "status"},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "телефон", width: 120, sortable: true, dataIndex: "phone"},
            {header: "моб.тел", width: 120, sortable: true, dataIndex: "mobilephone"},
            {header: "дом.тел", width: 120, sortable: true, dataIndex: "homephone"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "име", width: 120, sortable: true, dataIndex: "personname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "email", width: 120, sortable: true, dataIndex: "email"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"},
            {header: "сл.адрес", width: 120, sortable: true, dataIndex: "address1"},
            {header: "дом.адрес", width: 120, sortable: true, dataIndex: "address2"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text}'
        })

    });

    var ringgrid = new Ext.grid.GridPanel({
        border: false,
        store: ringlist,
        columns: [
            {header: "сценарий", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "телефон", width: 120, sortable: true, dataIndex: "phone"},
            {header: "моб.тел", width: 120, sortable: true, dataIndex: "mobilephone"},
            {header: "дом.тел", width: 120, sortable: true, dataIndex: "homephone"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "име", width: 120, sortable: true, dataIndex: "personname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "email", width: 120, sortable: true, dataIndex: "email"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"},
            {header: "сл.адрес", width: 120, sortable: true, dataIndex: "address1"},
            {header: "дом.адрес", width: 120, sortable: true, dataIndex: "address2"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text}'
        })
    });

    var reportstatus = new Ext.grid.GridPanel({
        border: false,
        store: report,
        columns: [
            {header: "резултат", width: 120, sortable: true, dataIndex: "state", hidden:"true"},
            {header: "статус", width: 120, sortable: true, dataIndex: "status"},
            {header: "id", width: 120, sortable: true, dataIndex: "id", hidden:"true"},
            {header: "телефон", width: 120, sortable: true, dataIndex: "phone"},
            {header: "моб.тел", width: 120, sortable: true, dataIndex: "mobilephone"},
            {header: "дом.тел", width: 120, sortable: true, dataIndex: "homephone"},
            {header: "организация", width: 120, sortable: true, dataIndex: "organisationname"},
            {header: "отдел", width: 120, sortable: true, dataIndex: "divisionname"},
            {header: "име", width: 120, sortable: true, dataIndex: "personname"},
            {header: "длъжност", width: 120, sortable: true, dataIndex: "positionname"},
            {header: "пункт", width: 120, sortable: true, dataIndex: "site"},
            {header: "email", width: 120, sortable: true, dataIndex: "email"},
            {header: "код", width: 120, sortable: true, dataIndex: "code"},
            {header: "група", width: 120, sortable: true, dataIndex: "ringgroupname"},
            {header: "сл.адрес", width: 120, sortable: true, dataIndex: "address1"},
            {header: "дом.адрес", width: 120, sortable: true, dataIndex: "address2"}
        ],
        view: new Ext.grid.GroupingView({
            groupTextTpl: '{text}'
        })
    });

    var queuegrid = new Ext.grid.GridPanel({
        border: false,
        store: queues,
        columns: [
            {header: "id", width: 30, sortable: true, dataIndex: "queueid", hidden:"true"},
            {header: "име", width: 120, sortable: true, dataIndex: "name"},
            {header: "време", width: 90, sortable: true, dataIndex: "time"}
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
                report.removeAll();
                report.load({
                    params:{mode:reportlist.mode,
                            fileid:grid.selModel.hasSelection()?grid.selModel.getSelected().id:0
                    }
                });
            }
        }
    });

    var textarea = new Ext.form.TextArea({
        border:false
    });

    var journal = new Ext.form.TextArea({
        border:false
    });

    var monitor_gsm = new Ext.Toolbar.TextItem("");
    var monitor_rs = new Ext.Toolbar.TextItem("");
    var monitor_sip = new Ext.Toolbar.TextItem("");
    var monitor_mail = new Ext.Toolbar.TextItem("");
    var monitor_mode = new Ext.Toolbar.TextItem("");

    report.on('load',function(){
        journal.setRawValue(report.reader.jsonData.log);
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
        title:"Стартиране на събитие",
        layout:"anchor",
        closeAction:"hide",
        modal: true,
        applyTo:"eventwindow",
        width:400,
        height:400,
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
                    height: 300,
                    minSize: 75,
                    margins: '0 5 5 5',
                    split: true,
                    layout: "fit",
                    items:[textarea]
                },{ 
                    title:'активни събития',
                    region:'west',
                    margins: '5 0 0 5',
                    cmargins: '5 5 0 5',
                    width: 270,
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
                    layout: "fit",
                    items:[grid]
                }]
                })
        ,
            new Ext.Panel({
				id: 'tabreports',
                listeners: {activate: function(){
						report.removeAll();
                        reportlist.removeAll();
                        reportlist.load();
                    }},
                title:'Справки',
                layout:'border',
                items:[{
                    region: 'south',
                    title:'журнал',
                    height: 300,
                    minSize: 75,
                    margins: '0 5 5 5',
                    split: true,
                    layout: "fit",
					items: [journal]
                },{ 
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
                    layout: "fit",
					items: [reportstatus]
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
                {text:"Промяна на режима",handler:changemode},
                {text:"Стартиране на събитие",handler:startevent},
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
        cleartask.cancel();
        if (tabs.activeTab.id=='tabmonitoring'){
			textarea.setRawValue('няма връзка със сървъра');
		}	
        monitor_mode.getEl().innerHTML="<span style='color:red;font-weight:bold'>Режим?</span>";
        monitor_rs.getEl().innerHTML="<span style='color:red;font-weight:bold'>RS?</span>";
        monitor_gsm.getEl().innerHTML="<span style='color:red;font-weight:bold'>GSM?</span>";
        monitor_mail.getEl().innerHTML="<span style='color:red;font-weight:bold'>Mail?</span>";
        monitor_sip.getEl().innerHTML="<span style='color:red;font-weight:bold'>SIP?</span>";
    });
    
	Ext.TaskMgr.start({
        run:function(){
            store.load({
                    callback:function(r,options,success){
                        if (success) cleartask.delay(3000);
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
			textarea.setRawValue(store.reader.jsonData.channels);
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

        if (store.reader.jsonData.monitor.gsm)
          monitor_gsm.getEl().innerHTML="<span style='color:green;font-weight:bold'>GSM</span>";
        else
          monitor_gsm.getEl().innerHTML="<span style='color:red;font-weight:bold'>GSM</span>";

        if (store.reader.jsonData.monitor.sip)
          monitor_sip.getEl().innerHTML="<span style='color:green;font-weight:bold'>SIP</span>";
        else
          monitor_sip.getEl().innerHTML="<span style='color:red;font-weight:bold'>SIP</span>";

        if (store.reader.jsonData.monitor.mail)
          monitor_mail.getEl().innerHTML="<span style='color:green;font-weight:bold'>Mail</span>";
        else
          monitor_mail.getEl().innerHTML="<span style='color:red;font-weight:bold'>Mail</span>";

        if (key) queuegrid.selModel.selectRow(store.find("queueid",key));
    });

    store.on('beforeload',function(){
	    if (tabs.activeTab.id=='tabmonitoring'){
			store.selectedkey=(grid.selModel.hasSelection())?grid.selModel.getSelected().id:null;
		}
    });

});