(function(){
   function enter(who, data){
     for (var u in this.ul) cast(u)("user_enter")(who, who);
     this.ul[who] = who;
     // this.ul[who] = data.nickname;
     cast(who)("users_refresh")(this.ul);
     cast(who)("msg_info")("hi "+who+", you are welcome");
   }
   function leave(who){
     delete this.ul[who];
     for (var u in this.ul) cast(u)("user_leave")(who);
   }
   function refresh(who){
     cast(who)("users_refresh")(this.ul);
   }
   function say(who, what){
     for (var u in this.ul) if(u != who) cast(u)("msg_said")(who, what);
     // cast(ul)("msg_said")(who, what);
   }
   function talk(who, whom, what){
     // debug("talk from:"+who+", to:"+whom+", say:"+what);
     cast(whom)("msg_told")(who, what);
   }
   return {ul:{}, enter:enter, leave:leave, refresh:refresh, say:say, talk:talk};
})();
