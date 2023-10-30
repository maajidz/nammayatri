import { callbackMapper } from 'presto-ui';

const { JOS, JBridge } = window;

var timerIdDebounce;
var inputForDebounce;
var timerIdForTimeout;
var allTimerIID = [];
var uniqueId = 0;

export const generateUniqueId = function (unit) {
  uniqueId += 1;
  return JSON.stringify(uniqueId);
};

export const getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};

export const getTimer = function (valType) {
  return function (startTime) {
  return function (inputVal) {
    return function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {

                console.log(valType+"<===>"+startTime+"<===>"+inputVal)

                var t = new Date().getTime()

                console.log("ttt-->>"+t)
                var countDownDate = (new Date(inputVal).getTime())-5000;

                console.log("countDownDate-->>"+countDownDate)

                if((Math.floor(((countDownDate - t) % (1000 * 60)) / 1000)) > 25000)
                {
                  countDownDate = (new Date().getTime())+ 20000;
                }

                instantGetTimer (function() {

                  // Get today's date and time
                  var now = new Date().getTime();

                  // Find the distance between now and the count down date
                  var distance = countDownDate - now;

                  // Time calculations for days, hours, minutes and seconds
                  var days = Math.floor(distance / (1000 * 60 * 60 * 24));
                  var hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
                  var minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
                  var seconds = Math.floor((distance % (1000 * 60)) / 1000);

                  // Display the result in the element with id="demo"
                  // document.getElementById("demo").innerHTML = days + "d " + hours + "h "
                  // + minutes + "m " + seconds + "s ";
                  // If the count down is finished, write some text
                 // console.log("Distance -->>")
                 // console.log(distance)
                 // console.log("-------------")
                  if (valType == "STOP"){
                    clearInterval(window.timerId);
                  }

                  if (distance <= 0) {
                    clearInterval(window.timerId);

                    console.log("EXPIRED");
                    cb(action("EXPIRED"))();
                    // return "EXPIRED";
                  }else{
                    if(days==0){
                      if(hours == 0){
                        if(minutes==0){
                          var timer = (seconds+1) + "s "
                        }else{
                          var timer = minutes + "m " + seconds + "s "
                        }
                      }else{
                        var timer = hours + "h "+ minutes + "m " + seconds + "s "
                      }
                    }else{
                      var timer = days + "d " + hours + "h "+ minutes + "m " + seconds + "s "
                    }

                  console.log(timer);
                  // return timer;
                  cb(action(timer))();
                }
              }, 1000);
              console.log("timerId",window.timerId);
            });
            // return JBridge.timerCallback(callback);
            window.callUICallback(callback);
        }

      }
    }
  }
}
}

export const countDown = function (countDownTime) {
  return function (id) {
    return function (cb) {
      return function (action) {
        return function () {
          var callback = callbackMapper.map(function () {
            var countDown = countDownTime;
            var timerIID = instantGetTimer(function () {
              countDown -= 1;
              if (countDown < 0) {
                //clearInterval(window.timerId);
                cb(action(0)(id)("EXPIRED")(timerIID))();
              } else {
                cb(action(countDown)(id)("INPROGRESS")(timerIID))();
              }
            }, 1000);
            // let timerId = setInterval(function () {
            //     countDown -= 1;
            //     if (countDown < 0) {
            //       cb(action(0)(id)("EXPIRED"))();
            //     } else {
            //       cb(action(countDown)(id)("INPROGRESS"))();
            //     }
            //   }, 1000);
            // setTimeout(() => { clearInterval(timerId); }, countDownTime*1000 + 1000);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}


export const clampNumber = function (number) {
  return function(originalMax) {
    return function(newMax) {
      const originalMin = 0;
      const newMin = 0;
      const originalRange = originalMax - originalMin;
      const newRange = newMax - newMin;
      
      const percentage = (number - originalMin) / originalRange;
      
      return Math.floor(newMin + percentage * newRange);
    }
  }
}

export const get15sTimer = function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {
                var seconds = 15;
                instantGetTimer (function() {
                  seconds = seconds - 1;
                  if (seconds <= 0) {
                    clearInterval(window.timerId);
                    console.log("EXPIRED");
                    cb(action("EXPIRED"))();
                  }else{
                    var timer = seconds + "s "
                    console.log(timer);
                    cb(action(timer))();
                }
              }, 1000);
              console.log("timerId",window.timerId);
            });
            window.callUICallback(callback);
        }

      }
    }

export const get5sTimer = function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {
                var time = 5;
                sayLetter(time);

                function sayLetter(seconds){
                 if (seconds >= 0)
                {
                     console.log(seconds+ "seconds")
                 setTimeout(() => {
                      if (seconds <= 0) {
                       clearInterval(window.timerId);
                      console.log("EXPIRED");
                         cb(action("EXPIRED"))();
                     }else{
                         var timer = seconds + "s "
                       console.log(timer);
                         cb(action(timer))();
                         sayLetter(seconds-1);
                       }
                     }, 1000);
                }
                }
                console.log("timerId",window.timerId);
            });
            window.callUICallback(callback);
        }
      }
    }

export const parseNumber = function (num) {
    num = num.toString();
    var lastThree = num.substring(num.length-3);
    var otherNumbers = num.substring(0,num.length-3);
    if(otherNumbers != '')
        lastThree = ',' + lastThree;
    var res = otherNumbers.replace(/\B(?=(\d{2})+(?!\d))/g, ",") + lastThree;
    return res;
}


export const get10sTimer = function (cb){
  return function (action) {
      return function(){
          var callback = callbackMapper.map(function () {
            var time = 10;
            sayLetter(time);

            function sayLetter(seconds){
             if (seconds >= 0)
            {
                console.log(seconds+ "seconds")
                setTimeout(() => {
                  if (seconds <= 0) {
                   clearInterval(window.timerId);
                  console.log("EXPIRED");
                     cb(action("EXPIRED"))();
                 }else{
                     var timer = seconds + "s "
                   console.log(timer);
                     cb(action(timer))();
                     sayLetter(seconds-1);
                   }
                 }, 1000);
            }
            }
            console.log("timerId",window.timerId);
        });
        window.callUICallback(callback);
    }
  }
}

export const startTimer = function(input) {
  return function (isCountDown) {
    return function (cb) {
      return function (action) {
        return function () {
          var callback = callbackMapper.map(function () {
            var time = input;
            time = time + (isCountDown ? -1 : 1);
            startCountDown(time);
            console.log("inside startTimer");
            function startCountDown(seconds) {
              if (seconds >= 0) {
                timerIdForTimeout = setTimeout(() => {
                  if (seconds <= 0) {
                    clearTimeout(timerIdForTimeout);
                    console.log("EXPIRED");
                    console.log(seconds + "seconds");
                    cb(action("EXPIRED"))();
                  } else {
                    var timer = seconds + "s ";
                    console.log(timer + "seconds");
                    cb(action(timer))();
                    startCountDown(seconds + (isCountDown ? -1 : 1));
                  }
                }, 1000);
              }
            }
            console.log("timerId : " + timerIdForTimeout);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}


export const clearTimer = function (a)
{ if(timerIdForTimeout){
    clearTimeout(timerIdForTimeout);
  }
  if(window.timerId){
    clearInterval(window.timerId);
  }
};

export const clearPopUpTimer = function (a) {
  clearInterval(parseInt(a));
};

function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  allTimerIID.push(window.timerId);
  return window.timerId;
}

export const clearAllTimer = function(a) {
  console.log("allTimerIID");
  console.log(allTimerIID);
  while(allTimerIID.length > 0){
    clearInterval(parseInt(allTimerIID.pop()));
  }
}
export const setRefreshing = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

export const setEnabled = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

export const decodeErrorCode = function (a) {
  try {
    var errorCodee = JSON.parse(a).errorCode;
    return  errorCodee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

export const decodeErrorMessage = function (a) {
  try {
    var errorMessagee = JSON.parse(a).errorMessage;
    if(errorMessagee == null)
    {
      return "";
    }
    return  errorMessagee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

export const convertKmToM = function (dist) {
  try{
    var distance = parseInt(dist);
    if (distance<1000)
    {
      return ((distance.toString()) + "m");
    }
    else
    {
      var disKm = distance/1000;
      return (((disKm.toFixed(1)).toString()) + "km");
    }
  }catch(e){
    console.log(e);
    console.log("error in convertKmToM----------------------------------");
  }
};

export const differenceBetweenTwoUTC = function (str1) {
  return function (str2) {
    var curr1 = new Date(str1);
    var departure = new Date(str2);
    console.log(departure + " , " + curr1 + "STR");
    var diff =(curr1.getTime() - departure.getTime())/ 1000;
    diff = (Math.round(diff));
    return diff
  };
};


export const storeCallBackForNotification = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (notificationType) {
            cb(action(notificationType))();
          });
          window.onResumeListeners = [];
          JBridge.storeCallBackForNotification(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackForNotification ------", error);
  }
}

export const getcurrentdate = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = yyyy + '-' + mm + '-' + dd;
  return today;
}

export const getDatebyCount = function (count) {
  var today = new Date();
  today.setDate(today.getDate() - count);
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = yyyy + '-' + mm + '-' + dd;
  return today;
}

export const hideSplash = JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()

export const currentPosition = function (str) {
  return function() {
    JBridge.currentPosition(str);
  }
}

export const toString = function (attr) {
  return JSON.stringify(attr);
};

export const getTime = function (unit){
  return Date.now();
}

export const toInt = function (val) {
  return parseInt(val);
}

export const secondsLeft = function (time){
  var validity = new Date(time).getTime();
  var now = new Date().getTime();
  if (validity <= now){
    return parseInt(1);
  }else{
    return parseInt((validity - now)/1000);
  }
}

export const objectToAllocationType = function (stringifyObject) {
  var json = JSON.parse(stringifyObject);
  console.log(json);
  return json;
}

export const storeCallBackTime = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (time, lat, lng) {
              cb(action(time)(lat)(lng))();
          });
          JBridge.storeCallBackTime(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackTime ------", error);
  }
}

export const launchAppSettings = function (unit) {
  return JBridge.launchAppSettings();
};

export const shuffle = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

export const setYoutubePlayer = function (json) {
  return function (viewId) {
    return function (videoStatus) {
        if (JBridge.setYoutubePlayer) {
          try {
            console.log("Inside setYoutubePlayer ------------");
            return JBridge.setYoutubePlayer(JSON.stringify(json), viewId, videoStatus);
          } catch (err) {
            console.log("error in setYoutubePlayer");
          }
        }
    };
  };
};

export const getTimeStampString = function (utcTime){
  var createdDate = new Date(utcTime);
  var currentDate = new Date();
  var diff = (currentDate.getTime() - createdDate.getTime())/ 1000;
  var seconds = (Math.round(diff));
  if (seconds <0) return "";
  var d = Math.floor(seconds / (3600*24));
  var h = Math.floor(seconds % (3600*24) / 3600);
  var m = Math.floor(seconds % 3600 / 60);
  var s = Math.floor(seconds % 60);
  if      (d > 0) return d + (d == 1 ? " day " : " days")
  else if (h > 0) return h + (h == 1 ? " hour " : " hours")
  else if (m > 0) return m + (m == 1 ? " minute " : " minutes")
  else            return s + (s == 1 ? " second" : " seconds")
}

export const clearFocus = function (id) {
    return JBridge.clearFocus(id)
}

export const addMediaPlayer = function (id) {
  return function(source) {
    return function () {
      JBridge.addMediaPlayer(id,source);
    }
  };
};

export const saveAudioFile = function (source) {
    return function() {
        return JBridge.saveAudioFile(source);
    }
}

export const uploadMultiPartData = function (path) {
    return function (url) {
        return function(fileType) {
            return function() {
                return JBridge.uploadMultiPartData(path, url, fileType);
            }
        }
    }
}

export const startAudioRecording = function (id) {
    return function() {
        return JBridge.startAudioRecording();
    }
};

export const stopAudioRecording = function (id) {
    return function() {
        return JBridge.stopAudioRecording();
    }
}

export const renderBase64ImageFile = function (base64Image) {
    return function(id) {
        return function (fitCenter) {
          return function (imgScaleType){
            return function () {
              try{
                return JBridge.renderBase64ImageFile(base64Image, id, fitCenter, imgScaleType);
              }catch (err){
                return JBridge.renderBase64ImageFile(base64Image, id, fitCenter);
              }
            }
          }  
        }
    }
}

export const removeMediaPlayer = function (id) {
  return function () {
    JBridge.removeMediaPlayer();
  };
};

export const getVideoID = function (url) {
  try {
    var ID = '';
    var updatedURL = url.replace(/(>|<)/gi, '').split(/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/|\/shorts\/)/);
    if (updatedURL[2] !== undefined) {
      ID = updatedURL[2].split(/[^0-9a-z_\-]/i);
      ID = ID[0];
    }
    else {
      if (updatedURL[1] == /shorts/) {
        ID = updatedURL[2];
      }else {
        ID = updatedURL;
      }
    }
    return ID;
  }catch (e) {
    console.log("error in getVideoID " + e);
  }
}

export const getImageUrl = function (url) {
  try {
    let videoId = getVideoID(url);
    return ("https://img.youtube.com/vi/" + videoId + "/maxresdefault.jpg");
  }catch (e) {
    console.log("error in getImageUrl " + e);
  }
};

export const getZoneTagConfig = function (just, nothing, key, zoneType){
  if (zoneType in zoneConfig){
    var zoneOb = zoneConfig[zoneType];
    if(key in zoneOb){
      var prop = zoneOb[key]
      return just(prop);
    }
    console.error("No value found for key "+ key);
  }
  console.error("No value found for key "+ zoneType);
  return nothing;
}

const zoneConfig = {
  SureMetro_Pickup : {
    "backgroundColor" : "#2194FF",
    "text" : "Metro Pickup",
    "imageUrl" : "ic_metro_white,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_metro_white.png",
    "cancelText" : "ZONE_CANCEL_TEXT_PICKUP",
    "cancelConfirmImage" : "ic_cancelride_metro_pickup,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_pickup.png"
  },
  SureMetro_Drop : {
    "backgroundColor" : "#2194FF",
    "text" : "Metro Drop",
    "imageUrl" : "ic_metro_white,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_metro_white.png",
    "cancelText" : "ZONE_CANCEL_TEXT_DROP",
    "cancelConfirmImage" : "ic_cancelride_metro_drop,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_drop.png"
  } //More zoneConfigs can be added
}

export const getPastDays = function (count) {
  try {
    let result = [];
    for (var i = 0; i < count; i++) {
      let d = new Date();
      d.setDate(d.getDate() - i);
      let obj = { utcDate: d.toISOString(), date: d.getDate(), month: d.toLocaleString('default', { month: 'short' }), year: d.getFullYear() };
      result.push(obj);
    }
    console.log(result);
    return result.reverse();
  } catch (e) {
    console.log("error in getPastDays", e);
  }
};

export const getPastWeeks = function (count) {
  try {
    let result = []
    var currentDate = new Date();
    while (currentDate.getDay() != 0) {
        currentDate.setDate(currentDate.getDate() - 1);
    }
    currentDate.setDate(currentDate.getDate() + 7);
    for (var i = 0; i < count; i++) {
      let dStart = new Date(currentDate);
      let dEnd = new Date(currentDate);
      dStart.setDate(dStart.getDate() - 7 * (i + 1));
      dEnd.setDate(dEnd.getDate() - (7 * i + 1));
      let obj = { utcStartDate: dStart.toISOString(), startDate: dStart.getDate(), utcEndDate: dEnd.toISOString(), endDate: dEnd.getDate(),
                  startMonth: dStart.toLocaleString('default', { month: 'short' }), endMonth: dEnd.toLocaleString('default', { month: 'short' }) }
      result.push(obj)
    }
    console.log(result);
    return result.reverse();
  } catch (e) {
    console.log("error in getPastWeeks", e);
  }
};

export const getPeriod = function(date) {
  var currentDate = new Date();
  var pastDate = new Date(date);
  var diff = Math.floor(currentDate.getTime() - pastDate.getTime());
  var days = Math.floor(diff/(1000 * 60 * 60 * 24));
  var months = Math.floor(days/31);
  var years = Math.floor(months/12);
  var period = years > 0 ? years : months > 0 ? months : days > 0 ? days : 0
  var periodType = years > 0 ? "Yrs" : months > 0 ? "Months" : days > 0 ? "Days" : "new"
  return {period : Math.abs(period)
  , periodType : periodType}
  }

export const getMerchantId = function(id) {
  return window.merchantID;
}

export const startPP = function (payload) {
	return function (sc) {
		return function () {
			var cb = function (code) {
				return function (_response) {
					return function () {
            var response = JSON.parse(_response);
						console.log("%cHyperpay Response ","background:darkblue;color:white;font-size:13px;padding:2px", response);                                                               
						sc(response.payload.payload.status.value0)();
					}
				}
			}

			if (JOS) {      
				try {
					payload = JSON.parse(payload);
          console.log("%cHyperpay Request ", "background:darkblue;color:white;font-size:13px;padding:2px", payload);
          let p = payload["payload"]
          Object.keys(p).forEach(key => {
           if (p[key] === null) {
             delete p[key];
           }
         });
         payload["payload"] = p;                
					console.log("%cHyperpay Request ", "background:darkblue;color:white;font-size:13px;padding:2px", payload);

					if (JOS.isMAppPresent("in.juspay.hyperpay")()){
            console.log("inside process call");
						JOS.emitEvent("in.juspay.hyperpay")("onMerchantEvent")(["process",JSON.stringify(payload)])(cb)();
					} else {
            sc("FAIL")();
					}
				} catch (err) {
					console.error("Hyperpay Request not sent : ", err);
				}
			}else{
            sc("FAIL")();
        }
		}
	}
}


export const initiatePP = function () {
  var cb = function (code) {
    return function (_response) {
      return function () {
        var response = JSON.parse(_response);
        console.log("%cHyperpay initiate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", response);
      }
    }
  }
  if (JOS) {
    try {
      console.log("%cHyperpay initiate RequestPP ", "background:darkblue;color:white;font-size:13px;padding:2px", window.__payload);
      JOS.startApp("in.juspay.hyperpay")(window.__payload)(cb)();
    } catch (err) {
      console.error("Hyperpay initiate Request not sent : ", err);
    }
  }
}

export const consumeBP = function (unit){
  var jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: true }
  }
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
}

export const isYesterday = function (dateString){
  try {
    const yesterday = new Date()
    yesterday.setDate(yesterday.getDate() - 1);
    var date = new Date(dateString);
    return (yesterday.toDateString() == date.toDateString());
  }catch(error){
    console.error(error);
  }
  return false;
}

export const getPopupObject = function (just, nothing, key){
  try {
    var val = JBridge.getFromSharedPrefs(key);
    if (val == "__failed") {
      return nothing;
    } 
    console.log("zxc console key ", val);
    console.log("zxc console parsed key ", JSON.parse(val));
    return just(JSON.parse(val));
  } catch( e ){
    console.warn(e);
  }
  return nothing;
}

export const checkPPInitiateStatus = function (cb) {
  if (JOS.isMAppPresent("in.juspay.hyperpay")() && window.isPPInitiated) {
    cb()();
  } else {
    window.ppInitiateCallback = cb;
  }
}

export const killPP = function () {
  if (top.JOSHolder) {
    const mapps = Object.keys(top.JOSHolder);
    mapps.forEach((key) => {
      if (key != JOS.self) {
        var currJOS = top.JOSHolder[key];
        currJOS.finish(0)("")();
        delete top.JOSHolder[key];
      }
    });
    window.ppInitiateCallback = undefined;
  }
}