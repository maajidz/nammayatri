export function getStringValue(key) {
  if (key in bengaliStrings) {
    return bengaliStrings[key];
  }
  console.error(key + " key not found in bengaliStrings");
  return "";
}

const bengaliStrings = {
  "DOWNLOAD_INVOICE": "চালান ডাউনলোড করুন",
  "REPORT_AN_ISSUE": "সমস্যা বিবৃতি করুন",
  "SUBMIT": "অনুমোদন",
  "VIEW_INVOICE": "চালান দেখুন",
  "TOTAL_AMOUNT": "মোট মূল্য",
  "AMOUNT_PAID": "পরিমাণ অর্থ প্রদান করা",
  "TRIP_DETAILS_": "ভ্রমণের বিশদ",
  "DOWNLOAD_PDF": "পিডিএফ ডাউনলোড করুন",
  "CGST": "সিজিএসটি",
  "INVOICE": "চালান",
  "TRIP_CHARGES": "ট্রিপ চার্জ",
  "PROMOTION": "পদোন্নতি",
  "SEND_EMAIL": "ইমেইল পাঠান",
  "YOU_CAN_DESCRIBE_THE_ISSUE_YOU_FACED_HERE": "আপনি এখানে যে সমস্যার মুখোমুখি হয়েছেন তা বর্ণনা বর্ণনা করতে পারেন",
  "THANK_YOU_FOR_WRITING": "আমাদের জানানোর জন্য আপনাকে ধন্যবাদ!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE": "আমরা আপনার সমস্যা জানতে পেয়েছি। আমরা কিছু সময় আপনার কাছে পৌঁছে যাব যাব।",
  "GO_HOME_": "গো হোমে",
  "LOGO": "লোগো",
  "ABOUT_APP_DESCRIPTION": "জাটি সাথি চালকদের সাথে রাইডারদের সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি রাইডারদের মিটার রেট সহ একটি যাত্রা বুক করা সুবিধাজনক করে তোলে তাই ন্যূনতম ভাড়া।",
  "ABOUT": "অ্যাপ সম্পর্কিত",
  "PRIVACY_POLICY": "গোপনীয়তা নীতি",
  "SET_UP_YOUR_ACCOUNT": "আপনার অ্যাকাউন্ট সেট আপ করুন",
  "CONTINUE": "চালিয়ে যান",
  "ENTER_YOUR_NAME": "আপনার নাম লিখুন",
  "FULL_NAME": "পুরো নাম",
  "EMAIL": "ইমেল",
  "WELCOME_TEXT": "জাত্রি সাথিতে আপনাকে স্বাগতম",
  "PLEASE_CHOOSE_YOUR_PREFERRED_LANGUAGE_TO_CONTINUE": "চালিয়ে যেতে আপনার পছন্দসই ভাষা চয়ন চয়ন করুন।।",
  "WRITE_TO_US": "আমাদের লিখুন",
  "NOTE": "বিঃদ্রঃ:",
  "VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS": "রাইড সংক্রান্ত অভিযোগের জন্য আমার রাইডস বিভাগে যান",
  "THANK_YOU_FOR_WRITING_TO_US": "আমাদের লেখার জন্য আপনাকে ধন্যবাদ!",
  "WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME": "আমরা আপনার সমস্যা পেয়েছি। আমরা কিছু সময় আপনার কাছে পৌঁছে যাব যাব।",
  "GO_TO_HOME__": "হোম পেজে যান",
  "SUBJECT": "বিষয়",
  "YOUR_EMAIL_ID": "আপনার ইমেল আইডি",
  "DESCRIBE_YOUR_ISSUE": "আপনার সমস্যা বর্ণনা করুন",
  "ENTER_MOBILE_NUMBER": "মোবাইল নম্বর লিখুন",
  "BY_TAPPING_CONTINUE": "ট্যাপ করে চালিয়ে যান",
  "TO_THE": "আপনি সম্মত হন যে আপনি আপনি গ্রহণ করছেন",
  "ENTER_OTP": "ওটিপি লিখুন",
  "RESEND": "আবার পাঠান",
  "ENTER_YOUR_MOBILE_NUMBER": "আপনার মোবাইল নম্বর লিখুন",
  "LOGIN_USING_THE_OTP_SENT_TO": "পাঠানো ওটিপি ব্যবহার করে লগইন করুন",
  "YOUR_RECENT_RIDE": "আপনার সাম্প্রতিক যাত্রা",
  "VIEW_ALL_RIDES": "সমস্ত রাইড দেখুন",
  "ALL_TOPICS": "সমস্ত বিষয়",
  "FAQ": "FAQ",
  "REPORT_AN_ISSUE_WITH_THIS_TRIP": "এই ট্রিপে একটি সমস্যা রিপোর্ট করুন",
  "YOU_RATED": "আপনি রেট করেছেন:",
  "GETTING_STARTED_AND_FAQS": "শুরু করা এবং প্রায়শই জিজ্ঞাসিত প্রশ্নাবলী",
  "FOR_OTHER_ISSUES_WRITE_TO_US": "অন্যান্য ইস্যুগুলির জন্য, আমাদের লিখুন",
  "HELP_AND_SUPPORT": "সাহায্য এবং সহযোগিতা",
  "OUR_SUGGESTED_PRICE_FOR_THIS_TRIP_IS": "এই ভ্রমণের জন্য আমাদের প্রস্তাবিত দাম",
  "DRIVERS_CAN_CHARGE_BETWEEN_THE_ABOVE_RANGE": "*ড্রাইভারগুলি উপরের পরিসরের মধ্যে চার্জ করতে পারে",
  "HOW_THIS_WORKS": "এটি কিভাবে কাজ করে?",
  "FINDING_RIDES_NEAR_YOU": "আপনার কাছাকাছি রাইড খোঁজা হচ্ছে...",
  "CONFIRMING_THE_RIDE_FOR_YOU": "আপনার জন্য রাইড নিশ্চিত করা হচ্ছে...",
  "CANCEL_SEARCH": "অনুসন্ধান বাতিল করুন",
  "YOUR_RIDE_IS_NOW_COMPLETE": "আপনার যাত্রা এখন সম্পূর্ণ!",
  "PLEASE_PAY_THE_FINAL_AMOUNT_TO_THE_DRIVER_VIA_CASH": "সরাসরি ড্রাইভারকে চূড়ান্ত পরিমাণ প্রদান করুন",
  "WHERE_TO": "কোথায়?",
  "HOME": "বাড়ি",
  "PICK_UP_LOCATION": "পিক আপ অবস্থান",
  "REQUEST_RIDE": "যাত্রা অনুরোধ",
  "NAME": "নাম",
  "MOBILE_NUMBER_STR": "মোবাইল নম্বর",
  "PERSONAL_DETAILS": "ব্যক্তিগত বিবরণ",
  "YOUR_RIDES": "আপনার রাইডস",
  "YOU_ARE_OFFLINE": "আপনি অফলাইন",
  "CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN": "আপনার ইন্টারনেট সংযোগ পরীক্ষা করুন এবং আবার আবার চেষ্টা করুন",
  "TRY_AGAIN": "আবার চেষ্টা করুন",
  "THANK_YOUR_DRIVER": "আপনার ড্রাইভারকে ধন্যবাদ!",
  "HOPE_YOUR_RIDE_WAS_HASSLE_FREE": "আমরা আশা করি আপনার যাত্রাটি ঝামেলা ঝামেলা মুক্ত ছিল ছিল",
  "HOW_WAS_YOUR_RIDE_WITH": "আপনার যাত্রা কেমন ছিল",
  "GOT_IT_TELL_US_MORE": "পেয়েছি, আরও বলুন?",
  "WRITE_A_COMMENT": "একটি মন্তব্য লিখুন (ঐচ্ছিক)",
  "UPDATE": "আপডেট",
  "LANGUAGE": "ভাষা",
  "OTP": "ওটিপি",
  "PAYMENT_METHOD": "অর্থপ্রদানের পদ্ধতি",
  "PAYMENT_METHOD_STRING": "নগদ / যেকোনো UPI অ্যাপ",
  "PAYMENT_METHOD_STRING_": "নগদ / UPI অ্যাপ ব্যবহার করুন",
  "CANCEL_RIDE": "যাত্রা বাতিল করুন",
  "SUPPORT": "সমর্থন",
  "PICKUP_AND_DROP": "পিকআপ এবং ড্রপ",
  "CANCELLED": "বাতিল করা হয়েছে",
  "HOW_THE_PRICING_WORKS": "মূল্য নির্ধারণ কিভাবে কাজ করে?",
  "SELECT_AN_OFFER": "একটি অফার নির্বাচন করুন",
  "CHOOSE_A_RIDE_AS_PER_YOUR_COMFORT": "আপনার আরাম অনুযায়ী একটি যাত্রা যাত্রা চয়ন করুন",
  "IT_SEEMS_TO_BE_A_VERY_BUSY_DAY": "খুব ব্যস্ত দিন মনে হচ্ছে। আপনি আবার রাইড খোঁজার চেষ্টা করতে পারেন",
  "SORT_BY": "ক্রমানুসার",
  "SORRY_WE_COULDNT_FIND_ANY_RIDES": "দুঃখিত, আমরা কোনও কোনও রাইড খুঁজে পাইনি",
  "LOAD_MORE": "আরো লোড করুন",
  "WE_NEED_ACCESS_TO_YOUR_LOCATION": "আমাদের আপনার অবস্থানের অ্যাক্সেস দরকার!",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM": "আপনার অবস্থান আমাদের সিস্টেমকে অটোর মাধ্যমে কাছাকাছি সমস্ত ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড প্রদান করতে সহায়তা করে।",
  "CALL": "ফোন করুন",
  "EMPTY_RIDES": "খালি রাইড",
  "YOU_HAVENT_TAKEN_A_TRIP_YET": "আপনি এখনও একটি ট্রিপ গ্রহণ করেননি",
  "BOOK_NOW": "এখনই বুক করুন",
  "T_AND_C_A": "ক) আপনি সম্মত হন যে আপনি বিটা পরীক্ষার একজন ইচ্ছুক অংশগ্রহণকারী এবং Juspay আপনার বিরুদ্ধে কোনো দায়বদ্ধতা থাকবে না",
  "TERMS_AND_CONDITIONS": "টি ও সি",
  "DATA_COLLECTION_AUTHORITY": "গ) আমি এতদ্বারা আমার তথ্য সংগ্রহ করার জন্য Juspay-কে নিয়োগ ও অনুমোদন করছি এবং চালিয়ে যাওয়ার মাধ্যমে, আমি ব্যবহারের শর্তাবলী এবং গোপনীয়তা নীতিতে সম্মত।",
  "DENY_ACCESS": "প্রবেশাধিকার অস্বীকার করুন",
  "PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL": "আপনি কেন বাতিল করতে চান দয়া করে করে আমাদের বলুন",
  "MANDATORY": "বাধ্যতামূলক",
  "SOFTWARE_LICENSE": "সফ্টওয়্যার লাইসেন্স",
  "LOGOUT_": "প্রস্থান",
  "REQUEST_AUTO_RIDE": "অনুরোধ যাত্রা",
  "RATE_YOUR_RIDE": "আপনার যাত্রা রেট",
  "SKIP": "এড়িয়ে যান",
  "ERROR_404": "ত্রুটি 404",
  "PROBLEM_AT_OUR_END": "আমাদের দিকে একটি সমস্যা আছে বলে মনে হচ্ছে. আমরা আবার উপলব্ধ হলে বিজ্ঞপ্তি পান",
  "NOTIFY_ME": "আমাকে অবহিত করুন",
  "ADDRESS": "ঠিকানা",
  "CHANGE": "পরিবর্তন",
  "SAVE_AS": "সংরক্ষণ করুন",
  "ADD_TAG": "ট্যাগ যুক্ত করুন",
  "WORK": "কাজ",
  "OTHER": "অন্য",
  "SAVE": "সংরক্ষণ",
  "ADD_NEW_ADDRESS": "নতুন ঠিকানা যোগ করুন",
  "SAVED_ADDRESSES": "সংরক্ষিত ঠিকানাগুলি",
  "ADDRESSES": "ঠিকানা",
  "NO_FAVOURITES_SAVED_YET": "এখনও কোনও প্রিয় সংরক্ষণ করা হয়নি",
  "SAVED_ADDRESS_HELPS_YOU_KEEP_YOUR_FAVOURITE_PLACES_HANDY": "প্রিয় অবস্থান আপনার ঘন ঘন পরিদর্শন করা জায়গাগুলি সহজ রাখতে সহায়তা করে",
  "COPIED": "কপি করা হয়েছে",
  "TRIP_ID": "ট্রিপ আইডি",
  "SAVE_PLACE": "জায়গা সংরক্ষণ করুন",
  "RIDE_FARE": "রাইড ভাড়া",
  "ASK_FOR_PRICE": "দাম জিজ্ঞাসা করুন",
  "ASK_FOR_PRICE_INFO": "চালকদের দ্বারা ভ্রমণ করা পিক-আপ দূরত্বের জন্য অতিরিক্ত 10 টাকা নামমাত্র ফি সহ আপনি সরকার নির্ধারিত মূল মূল্যের উপর ভিত্তি করে একটি ভাড়া পাবেন। কিছু ড্রাইভার শুধুমাত্র তাদের বিবেচনার ভিত্তিতে ট্রাফিক, রিটার্ন ট্রিপের সম্ভাবনা ইত্যাদি বিষয়গুলি কভার করার জন্য নামমাত্র টিপসের অনুরোধ করতে পারে।",
  "GET_ESTIMATE_FARE": "আনুমানিক ভাড়া পান",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS": "একটি অফার নির্বাচন করুন (ঐচ্ছিক)",
  "SELECT_AN_OFFER_FROM_OUR_DRIVERS_INFO": "ডিফল্টরূপে, যখন অটো-অ্যাসাইন একটি রাইড সক্ষম করা থাকে, তখন আপনাকে একজন ড্রাইভার নিয়োগ করা হয় যে অনুমান পরিসরের মধ্যে প্রথমে অনুরোধটি গ্রহণ করে। পরিবর্তে, আপনি যদি একটি ড্রাইভার অফার চয়ন করতে চান, তাহলে আপনি অক্ষম করতে পারেন এবং এগিয়ে যেতে পারেন।",
  "PAY_THE_DRIVER": "ড্রাইভারকে বেতন দিন",
  "PAY_THE_DRIVER_INFO": "ড্রাইভারকে সরাসরি অর্থ প্রদান করুন, যে মূল্য আপনি নিশ্চিত করেছেন",
  "PAY_THE_DRIVER_NOTE": "(রাইড দূরত্ব পরিবর্তন হলে মোট ভাড়া পরিবর্তন হতে পারে)",
  "UPDATE_PERSONAL_DETAILS": "ব্যক্তিগত বিবরণ আপডেট করুন",
  "EDIT": "এডিট করুন",
  "DEL_ACCOUNT": "অ্যাকাউন্ট মুছে ফেলুন",
  "ACCOUNT_DELETION_CONFIRMATION": "আপনি আপনার অ্যাকাউন্ট মুছে ফেলতে চান আপনি আপনি কি নিশ্চিত? আপনার সমস্ত ব্যক্তিগত ডেটা হারিয়ে যাবে",
  "REQUEST_SUBMITTED": "অনুরোধ জমা দেওয়া হয়েছে",
  "WE_WILL_DELETE_YOUR_ACCOUNT": "আপনি আমাদের প্ল্যাটফর্ম ছেড়ে চলে যেতে দেখে আমরা দুঃখিত। আপনার অ্যাকাউন্ট পরবর্তী 30 দিনের মধ্যে মুছে ফেলা হবে। এদিকে আপনি যদি আপনার অ্যাকাউন্টটি ধরে রাখতে চান তবে অনুগ্রহ করে আমাদের গ্রাহক সহায়তা নম্বরে কল করুন",
  "YES_DELETE_IT": "হ্যাঁ, এটি মুছুন",
  "REQUEST_TO_DELETE_ACCOUNT": "অ্যাকাউন্ট মুছে ফেলার জন্য অনুরোধ করুন",
  "CANCEL_STR": "বাতিল করুন",
  "LOADING": "লোড হচ্ছে",
  "PLEASE_WAIT_WHILE_IN_PROGRESS": "অনুগ্রহ করে প্রগতির সময় অপেক্ষা করুন",
  "SET_LOCATION_ON_MAP": "মানচিত্রে অবস্থান সেট করুন",
  "CURRENT_LOCATION": "বর্তমান অবস্থান",
  "ACTUAL_FARE_WAS_HIGHER_THAN_WHAT_WAS_SHOWN": "প্রকৃত ভাড়া যা দেখানো হয়েছিল তার চেয়ে বেশি ছিল।",
  "DELETE": "মুছে দিন",
  "ARE_YOU_SURE_YOU_WANT_TO_LOGOUT": "আপনি লগ আউট করতে চান ?",
  "ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "আপনি বাতিল করতে চান ?",
  "YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL": "আপনার রাইড অফার রয়েছে, আপনি কি নিশ্চিত নিশ্চিত যে আপনি বাতিল করতে করতে?",
  "GO_BACK_": "ফিরে যান",
  "REGISTER_USING_DIFFERENT_NUMBER": "আপনি কি আলাদা মোবাইল নম্বর ব্যবহার করে রেজিস্টার করতে চান?",
  "YES": "হ্যাঁ",
  "NO": "না",
  "CANCEL_": "বাতিল",
  "IS_ON_THE_WAY": "পথে আছে..",
  "ENTER_4_DIGIT_OTP": "4 ডিজিটের ওটিপি লিখুন",
  "WRONG_OTP": "ভুল ওটিপি",
  "GRANT_ACCESS": "অ্যাক্সেস মঞ্জুর করুন",
  "ENTER_A_LOCATION": "একটি অবস্থান লিখুন",
  "NEARBY": "কাছাকাছি",
  "MINS_AWAY": "মিনিট দূরে",
  "PAID": "প্রদত্ত",
  "BY_CASH": "নগদে",
  "ONLINE_": "অনলাইন",
  "USER": "ব্যবহারকারী",
  "EMAIL_ALREADY_EXISTS": "ইমেল আপডেট করতে ব্যর্থ। ই - মেইল   টি টি আগে থেকেই আছে আছে আছে",
  "IN": "ভিতরে",
  "VERIFYING_OTP": "ওটিপি যাচাই করা হচ্ছে",
  "TRACK_LIVE_LOCATION_USING": "ব্যবহার করে লাইভ অবস্থান ট্র্যাক করুন",
  "GOOGLE_MAP_": "গুগল মানচিত্র",
  "IN_APP_TRACKING": "অ্যাপ ট্র্যাকিং এ",
  "LIMIT_EXCEEDED": "সীমাবদ্ধতা ছাড়িয়ে গেছে",
  "QUOTE_EXPIRED": "ত্রুটি ঘটেছে",
  "GETTING_ESTIMATES_FOR_YOU": "উদ্ধৃতি মেয়াদ উত্তীর্ণ",
  "CONFIRM_PICKUP_LOCATION": "পিকআপের অবস্থান নিশ্চিত করুন",
  "CONFIRM_DROP_LOCATION": "ড্রপ অবস্থান নিশ্চিত করুন",
  "ERROR_OCCURED_TRY_AGAIN": "ত্রুটি ঘটেছে. আবার চেষ্টা করুন",
  "ASKED_FOR_MORE_MONEY": "আরও টাকা চেয়েছিলেন",
  "START_": "শুরু করুন",
  "RIDE_NOT_SERVICEABLE": "রাইড সেবাযোগ্য নয়",
  "APP_NOT_SERVICEABLE": "সেবাযোগ্য নয়",
  "CONFIRM_FOR": "জন্য নিশ্চিত করুন",
  "ETA_WAS_TOO_SHORT": "ইটিএ খুব ছোট ছিল।",
  "DRIVER_REQUESTED_TO_CANCEL": "ড্রাইভার আমাকে বাতিল করার জন্য অনুরোধ করেছিল",
  "PICK_UP_LOCATION_INCORRECT": "পিকআপের অবস্থানটি ভুল ছিল।",
  "COULD_NOT_CONNECT_TO_DRIVER": "আমি ড্রাইভারের সাথে সংযোগ করতে পারিনি।",
  "ETA_WAS_TOO_LONG": "ইটিএ খুব দীর্ঘ ছিল।",
  "OTHERS": "অন্যান্য",
  "DESTINATION_OUTSIDE_LIMITS": "প্রবেশ করা গন্তব্যটি শহরের সীমার বাইরে",
  "DROP_LOCATION_FAR_AWAY": "আপনার ড্রপের অবস্থানটি অনেক দূরে",
  "CHANGE_DROP_LOCATION": "ড্রপ অবস্থান পরিবর্তন করুন",
  "YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING": "আপনি রাইড বুকিং দিয়ে হাঁটতে বা চালিয়ে যেতে চাইতে পারেন",
  "YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST": "আপনার ভ্রমণের দূরত্ব খুবই কম। গন্তব্য ঠিক",
  "METERS_AWAY_FROM_YOUR_DESTINATION": "আমি দূরে!",
  "BOOK_RIDE_": "রাইড বুক করুন",
  "CANCEL_AUTO_ASSIGNING": "অটো অ্যাসাইনিং বাতিল করুন",
  "LOCATION_UNSERVICEABLE": "অবস্থান পরিষেবাযোগ্য নয়",
  "CURRENTLY_WE_ARE_LIVE_IN_": "আমরা এখনও আপনার এলাকায় বাস করিনি!\n\n আপনি মেনু থেকে রাইডের ইতিহাস এবং অন্যান্য সেটিংস অ্যাক্সেস করতে পারেন",
  "CHANGE_LOCATION": "অবস্থান পরিবর্তন করুন",
  "AUTO_ACCEPTING_SELECTED_RIDE": "স্বয়ংক্রিয়ভাবে গ্রহণ করা হচ্ছে",
  "THE_TRIP_IS_VERY_SHORT_AND_JUST_TAKE": "ট্রিপটি খুব সংক্ষিপ্ত এবং কেবল নিন",
  "IF_YOU_STILL_WANNA_BOOK_RIDE_CLICK_CONTINUE_AND_START_BOOKING_THE_RIDE": "আপনি যদি এখনও বুক করতে চান তাহলে চালিয়ে যান ক্লিক করুন এবং রাইড বুক করা শুরু করুন",
  "STEPS_TO_COMPLETE": "সম্পূর্ণ করার জন্য পদক্ষেপ",
  "HELP_US_WITH_YOUR_REASON": "আপনার কারণ দিয়ে আমাদের সহায়তা করুন",
  "MAX_CHAR_LIMIT_REACHED": "সর্বাধিক অক্ষর সীমা পৌঁছেছে",
  "DRIVER_WAS_NOT_REACHABLE": "ড্রাইভার পৌঁছনীয় ছিল না",
  "SHOW_ALL_OPTIONS": "সমস্ত বিকল্প দেখান",
  "EXPIRES_IN": "মেয়াদ শেষ",
  "PAY_DIRECTLY_TO_YOUR_DRIVER_USING_CASH_UPI": "*নগদ / ইউপিআই ব্যবহার করে সরাসরি আপনার ড্রাইভারকে অর্থ প্রদান করুন",
  "UPDATE_REQUIRED": "আপডেট প্রয়োজন",
  "PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE": "পরিষেবা চালিয়ে যেতে অ্যাপ্লিকেশন আপডেট করুন",
  "NOT_NOW": "এখন না",
  "OF": "এর",
  "LOST_SOMETHING": "কিছু হারিয়েছে?",
  "TRY_CONNECTING_WITH_THE_DRIVER": "ড্রাইভারের সাথে সংযোগ করার চেষ্টা করুন",
  "CALL_DRIVER": "ড্রাইভারকে কল করুন",
  "NO_MORE_RIDES": "আর কোনও রাইড নেই",
  "CONTACT_SUPPORT": "সহায়তার সাথে যোগাযোগ করুন",
  "INVALID_MOBILE_NUMBER": "অকার্যকর মোবাইল নম্বর",
  "RIDE_COMPLETED": "রাইড সম্পূর্ণ হয়েছে",
  "SUBMIT_FEEDBACK": "মতামত জমা দিন",
  "HOW_WAS_YOUR_RIDE_EXPERIENCE": "আপনার যাত্রার অভিজ্ঞতা কেমন ছিল?",
  "DROP": "ড্রপ",
  "RATE_YOUR_RIDE_WITH": "এর সাথে আপনার রাইড রেট করুন",
  "VIEW_BREAKDOWN": "ব্রেকডাউন দেখুন",
  "PAY_DRIVER_USING_CASH_OR_UPI": "নগদ/ইউপিআই ব্যবহার করে ড্রাইভারকে অর্থ প্রদান করুন",
  "PAY_DRIVER_USING_CASH_OR_UPI_": "নগদ/ইউপিআই ব্যবহার করে ড্রাইভারকে অর্থ প্রদান করুন",
  "RATE_YOUR_DRIVER": "আপনার ড্রাইভারকে রেট দিন",
  "MY_RIDES": "আমার রাইডস",
  "RIDE_DETAILS": "রাইডের বিবরণ",
  "RIDE_ID": "রাইড আইডি",
  "SELECT_A_RIDE": "আরো বিস্তারিত জানার জন্য একটি রাইড নির্বাচন করুন",
  "CONFIRM_RIDE_": "রাইড নিশ্চিত করুন",
  "YOU_CAN_CANCEL_RIDE": "ড্রাইভারদের কাছ থেকে অফার পাওয়ার পরে আপনি বাতিল করতে পারেন",
  "ESTIMATES_CHANGED": "আপনার যাত্রার অনুমান এখন পরিবর্তিত হয়েছে",
  "ESTIMATES_REVISED_TO": "সংশোধিত অনুমান হল",
  "RATE_CARD": "রেট কার্ড",
  "NIGHT_TIME_CHARGES": "রাতের সময় চার্জ",
  "MIN_FARE_UPTO": "পর্যন্ত সর্বনিম্ন ভাড়া",
  "MORE_THAN" : "কিলোমিটারেরও বেশি",
  "RATE_ABOVE_MIN_FARE": "ন্যূনতম ভাড়ার উপরে রেট",
  "DRIVER_PICKUP_CHARGES": "সেবা মূল্য",
  "NOMINAL_FARE": "নামমাত্র ভাড়া*",
  "DAY_TIMES_OF": "রাতে প্রযোজ্য দিনের ",
  "DAYTIME_CHARGES_APPLICABLE_AT_NIGHT": "x চার্জ থেকে - রাত ১০টা থেকে ভোর ৫টা",
  "NIGHT_TIMES_OF": "রাত 10 PM থেকে সকাল 5 AM পর্যন্ত ভাড়ার জন্য ",
  "DAYTIME_CHARGES_APPLIED_AT_NIGHT": "x দিনের চার্জ প্রযোজ্য (🌙)",
  "DRIVERS_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC": "* ড্রাইভার ট্রাফিক, রিটার্ন ট্রিপের সুযোগ ইত্যাদি কভার করার জন্য অতিরিক্ত উদ্ধৃতি দিতে পারে।",
  "GOT_IT": "বুঝেছি!",
  "DAY_TIME_CHARGES": "দিনের সময় চার্জ",
  "SHARE_APP": "অ্যাপ শেয়ার করুন",
  "AWAY_C": "দূরে",
  "AWAY": "দূরে",
  "AT_PICKUP": "পিকআপ এ",
  "FARE_UPDATED": "ভাড়া আপডেট হয়েছে",
  "TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE": "রুট পরিবর্তনের কারণে মোট ভাড়া পরিবর্তন হতে পারে",
  "AT_DROP": "ড্রপ এ",
  "EMERGENCY_HELP": "জরুরী সহায়তা",
  "CALL_POLICE": "পুলিশকে ফোন করুন",
  "ALSO_SHARE_YOUR_RIDE_STATUS_AND_LOCATION": "এছাড়াও আপনার রাইড স্ট্যাটাস এবং লোকেশন শেয়ার করে",
  "SHARE_RIDE_WITH_EMERGENCY_CONTACTS": "জরুরী পরিচিতিদের সাথে রাইড শেয়ার করুন",
  "DO_YOU_NEED_EMERGENCY_HELP": "আপনার কি জরুরি সহায়তা দরকার?",
  "CALL_SUPPORT": "সাপোর্টে কল করুন",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "আপনি নাম্মা ইয়াত্রি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
  "YOU_ARE_ABOUT_TO_CALL_YATRI_SATHI_SUPPORT": "You are about to place a call to the Yatri Sathi Support Team. Do you want to proceed?",
  "YOU_ARE_ABOUT_TO_CALL_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
  "YOU_ARE_ABOUT_TO_CALL_NEAREST_EMERGENCY_CENTRE": "আপনি নিকটতম জরুরি কেন্দ্রে কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
  "DIAL_112": "ডায়াল 112",
  "HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL": "আপনার মতামত দিয়ে আমাদের সহায়তা করুন (ঐচ্ছিক)",
  "WAIT_TIME": "অপেক্ষার সময়",
  "FAVOURITES": "প্রিয়",
  "ADD_FAVOURITE": "প্রিয় যোগ করুন",
  "ALL_FAVOURITES": "সমস্ত প্রিয়",
  "REMOVE": "অপসারণ করুন",
  "CONFIRM_LOCATION": "অবস্থান নিশ্চিত করুন",
  "SELECT_ON_MAP": "মানচিত্রে নির্বাচন করুন",
  "FAVOURITE_LOCATION": "প্রিয় অবস্থান",
  "EDIT_FAVOURITE": "প্রিয় এডিট করুন",
  "DRAG_THE_MAP": "মানচিত্রটি টেনে আনুন এবং সঠিক স্থানে পিন সেট করুন",
  "CHOOSE_ON_MAP": "মানচিত্রে চয়ন করুন",
  "USE_CURRENT_LOCATION": "বর্তমান অবস্থান ব্যবহার করুন",
  "FAVOURITE_YOUR_CURRENT_LOCATION": "আপনার বর্তমান অবস্থান পছন্দ করুন",
  "LOCATION": "অবস্থান",
  "LOCATION_ALREADY_EXISTS_AS": "অবস্থান ইতিমধ্যেই হিসাবে বিদ্যমান",
  "GIVE_THIS_LOCATION_A_NAME": "এই অবস্থানের একটি নাম দিন",
  "FAVOURITE": "প্রিয়",
  "CONFIRM_AND_SAVE": "নিশ্চিত করুন এবং সংরক্ষণ করুন",
  "REMOVE_FAVOURITE": "প্রিয় সরান",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_FAVOURITE_": "আপনি কি নিশ্চিত যে আপনি একটি প্রিয় অবস্থান সরাতে চান?",
  "YES_REMOVE": "হ্যাঁ, সরান",
  "ADD_NEW_FAVOURITE": "নতুন প্রিয় যুক্ত করুন",
  "SELECT_YOUR_DROP": "আপনার ড্রপ নির্বাচন করুন",
  "FAVOURITE_REMOVED_SUCCESSFULLY": "প্রিয় সফলভাবে সরানো হয়েছে",
  "LOCATION_ALREADY_EXISTS": "অবস্থান ইতিমধ্যে বিদ্যমান",
  "LOCATION_ALREADY": "ইতিমধ্যে অবস্থান",
  "EXISTS_AS": "হিসাবে বিদ্যমান",
  "FAVOURITE_ADDED_SUCCESSFULLY": "প্রিয় সফলভাবে যোগ করা হয়েছে",
  "FAVOURITE_UPDATED_SUCCESSFULLY": "প্রিয় সফলভাবে আপডেট করা হয়েছে",
  "ALREADY_EXISTS": "আগে থেকেই আছে",
  "NAME_ALREADY_IN_USE": "নাম ইতিমধ্যেই ব্যবহার করা হচ্ছে",
  "SELECT_FAVOURITE": "প্রিয় নির্বাচন করুন",
  "CONFIRM_CHANGES": "পরিবর্তনগুলি নিশ্চিত করুন",
  "ADD_SAVED_LOCATION_FROM_SETTINGS": "*আপনি সাইড মেনু> ফেভারিট থেকে নতুন প্রিয় যুক্ত করতে পারেন",
  "YOU_WILL_BE_ASKED_TO_SELECT_CONTACTS": "আপনাকে 3 টি পরিচিতি নির্বাচন করতে বলা হবে",
  "AUTO_ASSIGN_A_RIDE": "একটি যাত্রা অটো-অ্যাসাইন করুন",
  "IS_WAITING_FOR_YOU": "আপনার জন্য অপেক্ষা করছে...",
  "WAIT_TIME_TOO_LONG": "অপেক্ষার সময়টা খুব বেশি",
  "GOT_ANOTHER_RIDE_ELSE_WHERE": "অন্য কোথাও অন্য একটি যাত্রা পেয়েছি",
  "DRIVER_WAS_RUDE": "ড্রাইভার অভদ্র ছিল",
  "MAYBE_LATER": "পরে হতে পারে",
  "YOUR_RIDE_HAS_STARTED": "হ্যাঁ! আপনার যাত্রা শুরু হয়েছে 🤩",
  "ENJOY_RIDING_WITH_US": "আমাদের সাথে রাইডিং উপভোগ করছেন? শব্দটি ছড়িয়ে দিন এবং আনন্দ ভাগ করুন",
  "VIEW_DETAILS": "বিস্তারিত দেখুন",
  "REPEAT_RIDE": "রাইড পুনরাবৃত্তি করুন",
  "FARE_WAS_HIGH": "ভাড়া বেশি ছিল",
  "AUTO_ASSIGN_DRIVER": "একটি ড্রাইভারকে অটো-অ্যাসাইন করুন",
  "CHOOSE_BETWEEN_MULTIPLE_DRIVERS": "একাধিক ড্রাইভারের মধ্যে বেছে নিন",
  "CHOOSE_BETWEEN_MULTIPLE_RIDES": "একাধিক রাইড বিকল্পের মধ্যে বেছে নিন",
  "ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE": "আপনার পছন্দের রাইড এবং ভাড়া চয়ন করতে এই বৈশিষ্ট্যটি সক্রিয় করুন",
  "BOOKING_PREFERENCE": "বুকিং অগ্রাধিকার",
  "BASE_FARES": "বেস ভাড়া ",
  "PICKUP_CHARGE": "ড্রাইভার পিকআপ চার্জ",
  "TOTAL_PAID": "মোট প্রদত্ত",
  "WAITING_CHARGE": "অপেক্ষার চার্জ **",
  "NOMINAL_FARES": "নামমাত্র ভাড়া*",
  "DRIVERS_CAN_CHARGE_AN_ADDITIONAL_FARE_UPTO": "* ড্রাইভাররা ট্রাফিক, রিটার্ন ট্রিপের সম্ভাবনা ইত্যাদির মতো অন্যান্য বিষয়গুলি কভার করতে ₹20 পর্যন্ত অতিরিক্ত ভাড়া নিতে পারে।",
  "WAITING_CHARGE_DESCRIPTION": "** অপেক্ষার চার্জ: ₹ 1 / মিনিট - ড্রাইভার আসার 3 মিনিট পর",
  "SUCCESSFUL_ONBOARD": "আপনি সফলভাবে \n জাত্রি সাথিতে স্বাক্ষর করেছেন",
  "HAVE_REFERRAL_CODE": "একটি রেফারেল কোড আছে?",
  "REFEREAL_CODE_DISCRIPTION": "আপনার রেফারেলটি কেবল কোনও ড্রাইভারকে পুরস্কৃত করার কারণ হতে পারে!",
  "SIX_DIGIT_REFERRAL_CODE": "ড্রাইভার দ্বারা ভাগ করা 6 ডিজিট কোড লিখুন",
  "ABOUT_REFERRAL_PROGRAM": "রেফারাল প্রোগ্রাম কী?",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "রেফারাল প্রোগ্রামটি ড্রাইভারদের আরও রাইড গ্রহণ করতে, কম বাতিল করতে এবং যোগ্য ড্রাইভারদের স্বীকৃতি এবং পুরস্কৃত করে আপনাকে আরও ভাল পরিবেশন করতে উত্সাহিত করে। \n \n আপনি ড্রাইভারের রেফারেল কোডটি প্রবেশ করে এবং জাত্রি সাথী সম্প্রদায়ের জন্য রাইডের গুণমান উন্নত করে সহায়তা করতে পারেন!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n আপনি আপনার জাত্রি সাথী ড্রাইভারকে জিজ্ঞাসা করে একটি রেফারেল কোড পেতে পারেন।",
  "REFERRAL_CODE_SUCCESSFULL": "আপনি সফলভাবে প্রয়োগ করেছেন \n রেফারেল কোড!",
  "REFERRAL_CODE_APPLIED": "রেফারেল প্রয়োগ!",
  "HEY": "আরে",
  "INVALID_CODE_PLEASE_RE_ENTER": "ভুল সঙ্কেত. দয়া করে পুনরায় প্রবেশ করুন",
  "EMERGENCY_CONTACTS": "জরুরী যোগাযোগ",
  "ADD_EMERGENCY_CONTACTS": "জরুরী পরিচিতি যোগ করুন",
  "ADD_ANOTHER_CONTACT": "আরেকটি পরিচিতি যোগ করুন",
  "NO_EMERGENCY_CONTACTS_SET": "কোনো জরুরী পরিচিতি সেট নেই",
  "EMERGENCY_CONTACTS_SCREEN_DESCRIPTION": "আপনি একটি জরুরী পরিস্থিতিতে আপনার রাইড স্ট্যাটাস 3 জন পর্যন্ত জরুরি পরিচিতির সাথে শেয়ার করতে পারেন।",
  "CONTACTS_SELECTED": "পরিচিতি নির্বাচিত",
  "SELECT_CONTACTS": "পরিচিতি নির্বাচন করুন",
  "CONFIRM_EMERGENCY_CONTACTS": "জরুরী যোগাযোগ নিশ্চিত করুন",
  "ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT": "আপনি কি আপনার জরুরী পরিচিতিগুলি থেকে তাদের সরানোর বিষয়ে নিশ্চিত?",
  "SEARCH_CONTACTS": "পরিচিতি অনুসন্ধান করুন",
  "CALL_EMERGENCY_CONTACTS": "জরুরী পরিচিতি কল করুন",
  "LIVE_STATS_DASHBOARD": "লাইভ পরিসংখ্যান ড্যাশবোর্ড",
  "CHECK_OUT_LIVE_STATS": "লাইভ পরিসংখ্যান দেখুন",
  "CHOOSE_YOUR_RIDE" : "আপনার রাইড চয়ন করুন",
  "BOARD_THE_FIRST" : "যাত্রী সাথী জোন থেকে প্রথম",
  "TAXI" : "ট্যাক্সিতে ",
  "TAXI_FROM_ZONE" : "চড়ুন",
  "TAXI" : "ট্যাক্সিতে" ,
  "AC" : "এসি",
  "NON_AC" : "নন-এসি",
  "TAXI_FROM_ZONE" : "চড়ুন",
  "PAY_VIA_CASH_OR_UPI" : "নগদ / UPI এর মাধ্যমে অর্থ প্রদান করুন",
  "LET_TRY_THAT_AGAIN": "Let's try that again...",
  "EMERGENCY_CONTACS_ADDED_SUCCESSFULLY": "জরুরী যোগাযোগ সফলভাবে যোগ করা হয়েছে",
  "NO_CONTACTS_LEFT_ON_DEVICE_TO_ADD": "যোগ করার জন্য ডিভাইসে কোনো পরিচিতি অবশিষ্ট নেই",
  "PERCENTAGE_OF_NOMINAL_FARE": "~বেস ভাড়ার 10%",
  "ECONOMICAL" : "অর্থনৈতিক",
  "COMFY" : "আরামদায়ক",
  "NAVIGATE" : "নেভিগেট করুন",
  "GOVERNMENT_CHAGRES" : "রাইড জিএসটি (5%)",
  "SERVICE_CHARGES" : "সেবা মূল্য",
  "CONFIRM_AND_BOOK" : "নিশ্চিত করুন এবং বুক করুন",
  "PEOPLE" : "মানুষ",
  "CANCEL_ONGOING_SEARCH" : "আপনি কি নিশ্চিত যে আপনি চলমান অনুসন্ধান বাতিল করে চালিয়ে যেতে চান?",
  "SEARCH_AGAIN_WITH_A_TIP" : "আবার একটি টিপ দিয়ে অনুসন্ধান করবেন?",
  "TRY_AGAIN_WITH_A_TIP" : "একটি টিপ দিয়ে আবার চেষ্টা করুন?",
  "BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS" : "খুব ব্যস্ত দিন মনে হচ্ছে। রাইড পাওয়ার সম্ভাবনা বাড়ানোর জন্য আপনি একটি টিপ যোগ করার চেষ্টা করতে পারেন।",
  "TRY_AGAIN_WITHOUT_TIP" : "টিপ ছাড়াই আবার চেষ্টা করুন",
  "SEARCH_AGAIN_WITHOUT_A_TIP" : "টিপ ছাড়াই আবার অনুসন্ধান করুন",
  "TRY_AGAIN_WITH" : "আবার চেষ্টা করুন",
  "SEARCH_AGAIN_WITH" : "সাথে আবার অনুসন্ধান করুন",
  "TIP" : "টিপ",
  "CUSTOMER_SELECTED_FARE" : "গ্রাহক টিপ^",
  "START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS" : "এই দ্রুত চ্যাট পরামর্শগুলি ব্যবহার করে আপনার চ্যাট শুরু করুন" ,
  "MESSAGE" : "বার্তা",
  "START_YOUR_CHAT_WITH_THE_DRIVER" : "ড্রাইভারের সাথে আপনার চ্যাট শুরু করুন",
  "I_AM_ON_MY_WAY" : "আমি গন্তব্যের পথে",
  "GETTING_DELAYED_PLEASE_WAIT" : "বিলম্ব হচ্ছে, দয়া করে অপেক্ষা করুন",
  "UNREACHABLE_PLEASE_CALL_BACK" : "অ্যাক্সেসযোগ্য, দয়া করে ফিরে কল করুন",
  "ARE_YOU_STARING" : "আপনি কি শুরু করছেন?",
  "PLEASE_COME_SOON" : "দয়া করে শীঘ্রই আসুন",
  "OK_I_WILL_WAIT" : "আচ্ছা আমি অপেক্ষা করব",
  "I_HAVE_ARRIVED" : "আমি এসেছি",
  "PLEASE_COME_FAST_I_AM_WAITING" : "দয়া করে দ্রুত আসুন, আমি অপেক্ষা করছি",
  "PLEASE_WAIT_I_WILL_BE_THERE" : "দয়া করে অপেক্ষা করুন, আমি সেখানে থাকব",
  "LOOKING_FOR_YOU_AT_PICKUP" : "পিক-আপে আপনাকে খুঁজছেন",
  "MOBILE" : "মুঠোফোন",
  "HOW_DO_YOU_IDENTIFY_YOURSELF" : "আপনি নিজেকে কীভাবে চিহ্নিত করবেন?",
  "SELECT_YOUR_GENDER" : "আপনার লিঙ্গ নির্বাচন",
  "FEMALE" : "মহিলা",
  "MALE" : "পুরুষ",
  "PREFER_NOT_TO_SAY" : "বলতে না পছন্দ",
  "EMAIL_ID" : "ইমেইল আইডি",
  "SET_NOW" : "এখনই সেট করুন",
  "ADD_NOW" : "এখন যোগ করুন",
  "HOW_SHOULD_WE_ADDRESS_YOU" : "আমাদের কীভাবে আপনাকে সম্বোধন করা উচিত?",
  "GENDER_STR" : "লিঙ্গ",
  "PROFILE_COMPLETION" : "প্রোফাইল সমাপ্তি",
  "EARLY_END_RIDE_CHARGES" : "প্রারম্ভিক রাইড শেষ চার্জ^",
  "EARLY_END_RIDE_CHARGES_DESCRIPTION" : "Reard যাত্রা শেষে প্রথম দিকে অবরুদ্ধ দূরত্বের অর্ধেক ভাড়ার পরিমাণের অতিরিক্ত চার্জ দেয় (সর্বোচ্চ ₹ 50)",
  "YES_TRY_AGAIN" : "হ্যাঁ, আবার চেষ্টা করুন",
  "NO_DONT" : "না, না",
  "YES_CANCEL_SEARCH" : "হ্যাঁ, অনুসন্ধান বাতিল করুন",
  "TRY_LOOKING_FOR_RIDES_AGAIN" : "এটি খুব ব্যস্ত দিন বলে মনে হচ্ছে। আপনি আবার রাইড খুঁজছেন চেষ্টা করতে পারেন",
  "NO_TIP" : "₹0 টিপ্",
  "CUSTOMER_TIP_DESCRIPTION" : "^যাত্রা পাওয়ার সম্ভাবনা বাড়ানোর জন্য গ্রাহক কর্তৃক অতিরিক্ত পরিমাণ যুক্ত করা হয়েছে।",
  "PLACE_CALL" : "একটি কল করুন",
  "REQUEST_CALLBACK" : "কলব্যাকের অনুরোধ করুন",
  "DIRECT_CALL" : "সরাসরি কল",
  "ANONYMOUS_CALL" : "বেনামী কল",
  "YOUR_NUMBER_WILL_NOT_BE_SHOWN_TO_THE_DRIVER_THE_CALL_WILL_BE_RECORDED_FOR_COMPLIANCE" : "আপনার নম্বর ড্রাইভারকে দেখানো হবে না। সম্মতির জন্য কল রেকর্ড করা হবে।",
  "YOUR_NUMBER_WILL_BE_VISIBLE_TO_THE_DRIVER_USE_IF_NOT_CALLING_FROM_REGISTERED_NUMBER" : "আপনার নম্বরটি ড্রাইভারের কাছে দৃশ্যমান হবে। নিবন্ধিত নম্বর থেকে কল না করলে ব্যবহার করুন",
  "CALL_DRIVER_USING" : "ব্যবহার করে ড্রাইভারকে কল করুন",
  "WAS_YOUR_CALL_SUCCESSFUL" : "আপনার কল সফল ছিল" ,
  "GO_TO_ZONE" : "যাত্রী সাথী জোনে যান",
  "REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON" : "অনুরোধ পেয়েছি. আমরা তোমার সাথে শীঘ্রই যোগাযোগ করবো",
  "CONTACT_REMOVED_SUCCESSFULLY" : "যোগাযোগ সফলভাবে সরানো হয়েছে",
  "CORPORATE_ADDRESS" : "অভিযোগ এবং অভিযোগ",
  "CORPORATE_ADDRESS_DESCRIPTION" : "জুসপ্যায় টেকনোলজিস প্রাইভেট লিমিটেড <বড়> গিরিজা বিল্ডিং, নম্বর ৮১৭, গ্যানাপ্যাথি টেম্পলে রড, ৮থ ব্লক, কোরামাঙ্গালা, বেঙ্গালুরু, কর্ণাটক ৫৬০০৯৫, ইন্ডিয়া",
  "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "অভিযোগের প্রতিকারের জন্য, অনুগ্রহ করে আমাদের <u>গোপনীয়তা নীতি</u> পড়ুন",
  "REGISTERED_ADDRESS" : "নিবন্ধিত ঠিকানা",
  "REGISTERED_ADDRESS_DESCRIPTION" : "জুসপ্যায় টেকনোলজিস প্রাইভেট লিমিটেড <বড়> গিরিজা বিল্ডিং, নম্বর ৮১৭, গ্যানাপ্যাথি টেম্পলে রড, ৮থ ব্লক, কোরামাঙ্গালা, বেঙ্গালুরু, কর্ণাটক ৫৬০০৯৫, ইন্ডিয়া",
  "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
  "DRIVER_ADDITIONS": "ড্রাইভার সংযোজন*",
  "FARE_UPDATE_POLICY": "ভাড়া আপডেট নীতি",
  "UPDATE_NOW" : "এখন হালনাগাদ করুন",
  "DRIVER_ADDITIONS_OPTIONAL": "ড্রাইভার সংযোজন (ঐচ্ছিক)",
  "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC" : "ড্রাইভার ট্রাফিক কভার করার জন্য অতিরিক্ত উদ্ধৃত করতে পারে, ফিরতি ট্রিপের সুযোগ ইত্যাদি।",
  "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE" : "ড্রাইভার সংযোজনের সীমা বেস ভাড়ার 10% এ গণনা করা হয় নিকটতম ₹10 এ রাউন্ড অফ",
  "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE" : "দ্রষ্টব্য: ড্রাইভার এই অতিরিক্ত ভাড়া নিতে পারে/নাও পারে",
  "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS": "নিচের যেকোনো কারণে আপনি একটি আপডেট করা চূড়ান্ত ভাড়া দেখতে পারেন:",
  "REASON_CHANGE_IN_ROUTE_A": "1. রুটে পরিবর্তন: ",
  "REASON_CHANGE_IN_ROUTE_B": "রুট পরিবর্তনের কারণে মোট ভাড়া পরিবর্তন হতে পারে",
  "RECOMMENDED" : "প্রস্তাবিত",
  "COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE" : "ব্যক্তিগতকৃত রাইড অভিজ্ঞতার জন্য আপনার প্রোফাইল সম্পূর্ণ করুন",
  "WE_WOULD_APPRECIATE_YOUR_FEEDBACK" : "আমরা অ্যাকাউন্ট মুছে ফেলার পিছনে আপনার যুক্তির প্রশংসা করব",
  "REASON_FOR_DELETING_ACCOUNT" : "অ্যাকাউন্ট মুছে ফেলার কারণ*",
  "SUBMIT_REQUEST" : "অনুরোধ জমা দিন",
  "PLEASE_ENTER_A_VALID_EMAIL" : "একটি বৈধ ইমেইল প্রবেশ করুন",
  "WE_WOULD_APPRECIATE_YOUR_REASONING" : "অ্যাকাউন্ট মুছে ফেলার পিছনে আপনার যুক্তি সম্পর্কে আমরা আপনার প্রতিক্রিয়ার প্রশংসা করব",
  "OK_GOT_IT" : "ঠিক আছে বুঝেছি",
  "WAIT_FOR_DRIVER" : "ড্রাইভারের জন্য অপেক্ষা করুন",
  "NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS" : "পরিকল্পনা পরিবর্তনের কারণে আর যাত্রার প্রয়োজন নেই",
  "CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP" : "আমি অন্য অ্যাপে রাইড পেয়েছি বলে বাতিল করা হচ্ছে",
  "DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP" : "ম্যাপে ড্রাইভারের অবস্থান পরিবর্তন করা হয়নি",
  "DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION" : "চালক পিকআপ অবস্থানে পৌঁছতে অনেক সময় নিচ্ছিলেন",
  "THE_PICKUP_LOCATION_ENTERED_WAS_WRONG" : "প্রবেশ করা পিকআপ অবস্থান ভুল ছিল",
  "YOUR_DRIVER_IS_JUST" : "আপনার ড্রাইভার মাত্র ",
  "M_AWAY" : " মি দূরে",
  "DRIVER_HAS_ALREADY_TRAVELLED" : "ড্রাইভার ইতিমধ্যে ",
  "PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING" : "\nবাতিল করার আগে ড্রাইভারের সাথে যোগাযোগ করুন",
  "CHANGE_OF_PLANS" : "পরিকল্পনার পরিবর্তন",
  "DRIVER_IS_NOT_MOVING" : "চালক নড়ছে না",
  "WRONG_PICKUP_LOCATION" : "ভুল পিক আপ অবস্থান",
  "DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE" : "চালক হয়তো বিকল্প পথ নিচ্ছেন।",
  "DRIVER_IS_NOT_MOVING_Q" : "চালক নড়ছে না?",
  "WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING" : "\nআপনি বাতিল করার আগে ড্রাইভারের সাথে চেক করতে চান?",
  "DRIVER_IS_NEAR_YOUR_LOCATION" : "ড্রাইভার আপনার অবস্থানের কাছাকাছি আছে.",
  "SOME_OTHER_REASON" : "অন্য কোনো কারণ।",
  "HAS_TRAVELLED" : " ভ্রমণ করেছে",
  "METRO_RIDE": "মেট্রো রাইড",
  "GO_BACK_TEXT": "ফিরে যাও",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST": "ড্রাইভার আপনার বিশেষ অনুরোধ পছন্দ করেছে এবং মাত্র ",
  "DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST": "ড্রাইভার আপনার বিশেষ অনুরোধকে অগ্রাধিকার দিয়েছে এবং ইতিমধ্যে ",
  "AND_HAS_TRAVELLED": " ভ্রমণ করেছে।",
  "PLEASE_FIND_REVISED_FARE_ESTIMATE": "অনুগ্রহ করে সংশোধিত আনুমানিক ভাড়া খুঁজুন। রাতের চার্জ দিনের চার্জের 1.5 গুণ।",
  "GO_TO_ZONE" : "যাত্রী সাথী জোনে যান" ,
  "FARE_ESTIMATE" : "ভাড়া অনুমান" ,
  "TIP_SELECTED" : "টিপ নির্বাচিত",
  "ADD_A_TIP_TO_FIND_A_RIDE_QUICKER" : "দ্রুত একটি রাইড খুঁজে পেতে একটি টিপ যোগ করুন!",
  "IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL" : "এটি স্বাভাবিকের চেয়ে বেশি সময় নিচ্ছে বলে মনে হচ্ছে।",
  "CONTINUE_SEARCH_WITH" : "এর সাথে অনুসন্ধান চালিয়ে যান",
  "CONTINUING_SEARCH_WITH" : "সঙ্গে অনুসন্ধান অব্যাহত",
  "SEARCHING_WITH" : "সঙ্গে অনুসন্ধান",
  "THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "ড্রাইভার আপনার বিশেষ অবস্থান পছন্দ করেছে এবং ইতিমধ্যেই আপনার অবস্থানের পথে রয়েছে৷",
  "ALLOW_LOCATION_ACCESS": "অবস্থান অ্যাক্সেসের অনুমতি দিন",
  "MESSAGE_FROM_DRIVER": "ড্রাইভার থেকে বার্তা",
  "REPLY": "উত্তর দিন",
  "NAME_SHOULD_BE_MORE_THAN_2_CHARACTERS" : "নাম 2 অক্ষরের বেশি হওয়া উচিত",
  "THIS_FIELD_IS_REQUIRED" : "ঘরটি অবশ্যই পূরণ করতে হবে",
  "OKAY_GOT_IT" : "আচ্ছা আমি বুঝে গেছি",
  "CALL_NAMMA_YATRI_SUPPORT" : "নম্মা যাত্রী সমর্থনে কল করুন",
  "CALL_112": "112 কল করুন",
  "CALL_EMERGENCY_CENTRE": "ইমার্জেন্সি সেন্টারে কল করুন",
  "DRIVER_ADDITION_LIMITS_ARE_IN_INCREMENTS" : "ড্রাইভার যোগ করার সীমা ₹10 বৃদ্ধিতে রয়েছে ",
  "SEATS" : "আসন",
  "HATCHBACK" : "হ্যাচব্যাক",
  "SUV" : "এসইউভি",
  "SEDAN" : "সেডান",
  "CHANGE_OF_PLANS" : "পরিকল্পনার পরিবর্তন",
  "DRIVER_IS_NOT_MOVING" : "চালক নড়ছে না",
  "WRONG_PICKUP_LOCATION" : "ভুল পিক আপ অবস্থান",
  "DRIVER_MIGHT_BE_TAKING_ALTERNATE_ROUTE" : "চালক হয়তো বিকল্প পথ নিচ্ছেন।",
  "DRIVER_IS_NOT_MOVING_Q" : "চালক নড়ছে না?",
  "WOULD_YOU_LIKE_TO_CHECK_WITH_THE_DRIVER_BEFORE_CANCELLING" : "\nআপনি বাতিল করার আগে ড্রাইভারের সাথে চেক করতে চান?",
  "DRIVER_IS_NEAR_YOUR_LOCATION" : "ড্রাইভার আপনার অবস্থানের কাছাকাছি আছে.",
  "SOME_OTHER_REASON" : "অন্য কোনো কারণ।",
  "NO_LONGER_REQUIRE_A_RIDE_DUE_TO_CHANGE_IN_PLANS" : "পরিকল্পনা পরিবর্তনের কারণে আর যাত্রার প্রয়োজন নেই",
  "CANCELLING_AS_I_GOT_A_RIDE_ON_ANOTHER_APP" : "আমি অন্য অ্যাপে রাইড পেয়েছি বলে বাতিল করা হচ্ছে",
  "DRIVER_LOCATION_WASNT_CHANGING_ON_THE_MAP" : "ম্যাপে ড্রাইভারের অবস্থান পরিবর্তন করা হয়নি",
  "DRIVER_WAS_TAKING_TOO_LONG_TO_REACH_THE_PICKUP_LOCATION" : "চালক পিকআপ অবস্থানে পৌঁছতে অনেক সময় নিচ্ছিলেন",
  "THE_PICKUP_LOCATION_ENTERED_WAS_WRONG" : "প্রবেশ করা পিকআপ অবস্থান ভুল ছিল",
  "YOUR_DRIVER_IS_JUST" : "আপনার ড্রাইভার মাত্র ",
  "M_AWAY" : " মিটার দূরে.",
  "DRIVER_HAS_ALREADY_TRAVELLED" : "ড্রাইভার ইতিমধ্যে ",
  "HAS_TRAVELLED" : " ভ্রমণ করেছেন",
  "PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING" : "\nবাতিল করার আগে ড্রাইভারের সাথে যোগাযোগ করুন",
  "WAIT_FOR_DRIVER" : "ড্রাইভারের জন্য অপেক্ষা করুন",
  "DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION" : "ড্রাইভার ইতিমধ্যে আপনার অবস্থানের পথে আছে.",
  "OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN" : "OTP পৃষ্ঠার মেয়াদ শেষ হয়ে গেছে, অনুগ্রহ করে আবার OTP অনুরোধ করুন",
  "OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER" : "OTP ইনপুট সীমা পৌঁছে গেছে, অনুগ্রহ করে পরে আবার চেষ্টা করুন",
  "TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER" : "আপনি অনেকগুলি অবৈধ প্রচেষ্টা করেছেন, অনুগ্রহ করে পরে আবার চেষ্টা করুন৷",
  "SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN" : "कुछ तकनीकी समस्या हुई है, कृपया पुनः प्रयास करें",
  "SORRY_LIMIT_EXCEEDED_YOU_CANT_ADD_ANY_MORE_FAVOURITES" : "দুঃখিত, সীমা অতিক্রম করেছে. আপনি আর কোন প্রিয় নম্বর যোগ করতে পারবেন না",
  "IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_BOOKING_PLEASE_RESTART_THE_APPTO_VIEW_IT" : "আপনার ইতিমধ্যেই একটি বুকিং চলছে, এটি দেখতে অ্যাপটি আবার খুলুন।",
  "CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN" : "বাতিল করা ব্যর্থ হয়েছে, আবার চেষ্টা করুন",
  "NO_DRIVER_AVAILABLE_AT_THE_MOMENT_PLEASE_TRY_AGAIN" : "এই সময়ে কোন ড্রাইভার উপলব্ধ নেই, অনুগ্রহ করে আবার চেষ্টা করুন",
  "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "যাত্রী সাথী এলাকার জন্য ওটিপির মেয়াদ শেষ হয়ে গেছে, অনুগ্রহ করে বুকিংয়ের জন্য আবার চেষ্টা করুন",
  "NO_CONTACTS_FOUND_ON_THE_DEVICE_TO_BE_ADDED" : "ডিভাইসে কোনো পরিচিতি পাওয়া যায়নি",
  "PLEASE_ENABLE_CONTACTS_PERMISSION_TO_PROCEED" : "অনুগ্রহ করে এগিয়ে যাওয়ার জন্য পরিচিতির অনুমতি সক্ষম করুন",
  "LIMIT_REACHED_3_OF_3_EMERGENCY_CONTACTS_ALREADY_ADDED" : "সীমা ছাড়িয়ে গেছে! 3টির মধ্যে 3টি জরুরি পরিচিতি ইতিমধ্যেই যোগ করা হয়েছে৷",
  "INVALID_CONTACT_FORMAT" : "অবৈধ যোগাযোগ বিন্যাস",
  "OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER" : "OTP পুনরায় পাঠানোর সীমা পৌঁছে গেছে, অনুগ্রহ করে পরে আবার চেষ্টা করুন",
  "EMAIL_EXISTS_ALREADY" : "ই - মেইল টি আগে থেকেই আছে",
  "RATE_YOUR_EXPERIENCE" : "আপনার অভিজ্ঞতা রেট",
  "REPORT_ISSUE_" : "রিপোর্ট সমস্যা",
  "DONE" : "সম্পন্ন",
  "PLEASE_TELL_US_WHAT_WENT_WRONG" : "কি ভুল হয়েছে আমাদের বলুন",
  "DID_YOU_FACE_ANY_ISSUE" : "আপনি কোন সমস্যা সম্মুখীন?",
  "WE_NOTICED_YOUR_RIDE_ENDED_AWAY" : "আমরা লক্ষ্য করেছি যে আপনার রাইড আপনার আসল গন্তব্য থেকে দূরে শেষ হয়েছে৷",
  "GET_CALLBACK_FROM_US" : "আমাদের কাছ থেকে একটি কলব্যাক পান",
  "DRIVER_WAS_NOT_READY_TO_GO" : "ড্রাইভার যেতে প্রস্তুত ছিল না",
  "ASKING_FOR_MORE_MONEY" : "চালক আরও টাকা চাইছিলেন",
  "AUTO_BROKEN" : "অটো ভেঙে পড়ে",
  "WE_WILL_GIVE_YOU_CALLBACK" : "আমরা আপনাকে 24 ঘন্টার মধ্যে একটি কলব্যাক দেব",
  "YOUR_ISSUE_HAS_BEEN_REPORTED" : "আপনার সমস্যা সফলভাবে রিপোর্ট করা হয়েছে",
  "OTP_RESENT_SUCCESSFULLY" : "OTP সফলভাবে পুনরায় পাঠানো হয়েছে",
  "DESCRIPTION_SHOULD_BE_MORE_THAN_10_ALPHABETIC_CHARACTERS" : "বর্ণনা 10টির বেশি বর্ণমালার অক্ষর হওয়া উচিত",
  "INCORRECT_OTP_PLEASE_TRY_AGAIN" : "ভুল OTP, আবার চেষ্টা করুন.",
  "N_MORE_ATTEMPTS_LEFT" : " আরো প্রচেষ্টা বাকি",
  "IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_" : "মনে হচ্ছে আপনার একটি চলমান বুকিং আছে, এটি দেখতে অ্যাপটি পুনরায় চালু করুন",
  "GO_TO_SELECTED_PICKUP_SPOT" : "নির্বাচিত পিকআপ স্পটে যান",
  "GO_TO_SELECTED_PICKUP_SPOT_AS_AUTOS_ARE_RESTRICTED" : "প্রস্তাবিত পিকআপ অবস্থানে ড্রাইভ করুন কারণ এই এলাকার ভিতরে অটোগুলি সীমাবদ্ধ",
  "UNPROFESSIONAL_DRIVER": "অপেশাদার চালক",
  "RASH_DRIVING": "বেপর্দা গাড়ি চালানো",
  "DRIVER_CHARGED_MORE": "ড্রাইভার অতিরিক্ত চার্জ নেয়া",
  "UNCOMFORTABLE_AUTO": "অসুবিধাজনক অটো",
  "TRIP_GOT_DELAYED": "ট্রিপ বিলম্বিত হয়েছে",
  "FELT_UNSAFE": "অসুরক্ষিত অনুভূতি",
  "POLITE_DRIVER": "শিষ্ট চালক",
  "EXPERT_DRIVING": "দক্ষ ড্রাইভিং",
  "SAFE_RIDE": "নিরাপদ ভ্রমণ",
  "CLEAN_AUTO": "পরিষ্কার অটো",
  "ON_TIME": "সময়ে",
  "SKILLED_NAVIGATOR": "দক্ষ নেভিগেটর",
  "RUDE_DRIVER": "অশিষ্ট চালক",
  "TOO_MANY_CALLS": "বেশি কল",
  "RECKLESS_DRIVING": "অব্যবস্থিত চালনা",
  "LATE_DROP_OFF": "বিলম্বিত ড্রপ অফ",
  "LATE_PICK_UP": "বিলম্বিত পিক আপ",
  "POOR_EXPERIENCE": "খারাপ অভিজ্ঞতা 😕",
  "TERRIBLE_EXPERIENCE": "ভয়ানক অভিজ্ঞতা 😠",
  "NEEDS_IMPROVEMENT": "উন্নতি প্রয়োজন 😕",
  "ALMOST_PERFECT": "প্রায় নির্দিষ্ট! 🙂",
  "AMAZING": "আশ্চর্যজনক!!! 🤩",
  "ASKED_FOR_EXTRA_FARE": "বাড়তি ভাড়া চেয়েছেন",
  "ANYTHING_THAT_YOU_WOULD_LIKE_TO_TELL_US" : "আপনি আমাদের বলতে চান যে কিছু? (ঐচ্ছিক)",
  "PLEASE_WAIT" : "অনুগ্রহপূর্বক অপেক্ষা করুন",
  "EMAIL_EXISTS_ALREADY" : "ই - মেইল ​​টি আগে থেকেই আছে",
  "PLATFORM_FEE" : "প্ল্যাটফর্ম ফি",
  "EMAIL_EXISTS_ALREADY" : "ই - মেইল ​​টি আগে থেকেই আছে",
  "PLATFORM_FEE" : "প্ল্যাটফর্ম ফি",
  "SGST" : "এসজিএসটি",
  "OTP_EXPIRED" : "OTP মেয়াদ শেষ",
  "OTP_EXPIRED_DESCRIPTION" : "আপনার রাইডের OTP মেয়াদ শেষ হয়ে গেছে। একটি রাইড পেতে আবার বুক করুন",
  "PLATFORM_GST": "কর (GST)",
  "MISC_WAITING_CHARGE": "বিবিধ (ওয়েটিং চার্জ সহ)",
  "AC_TAXI": "AC ট্যাক্সি",
  "NON_AC_TAXI": "কোন এসি ট্যাক্সি নেই",
  "GET_OTP_VIA_WHATSAPP" : "হোয়াটসঅ্যাপের মাধ্যমে ওটিপি পান",
  "OR" : "বা",
  "HELPS_DRIVER_CONFIRM_ITS_YOU" : "(ড্রাইভার নিশ্চিত করতে সাহায্য করে যে এটি আপনিই)",
  "LETS_GET_YOU_TRIP_READY" : "চলুন আপনাকে ভ্রমণের জন্য প্রস্তুত করি!",
  "GOT_AN_OTP" : "একটি OTP পেয়েছেন?",
  "JUST_ONE_LAST_THING" : "শুধু একটি শেষ জিনিস",
  "TOLL_CHARGES_WILL_BE_EXTRA" : "টোল/পার্কিং চার্জ অতিরিক্ত হবে",
  "AUTO_RICKSHAW" : "অটো রিক্সা",
  "CABS_AVAILABLE" : "ক্যাব উপলব্ধ",
  "GENERAL_DISABILITY_DESCRIPTION" : "ড্রাইভারদেরকে আপনার প্রয়োজন এবং অনুরোধে সাহায্য করতে উৎসাহিত করা হবে।",
  "PI_POINTER_1" : "ড্রাইভারদের আপনার পিকআপের জন্য আপনার নির্দিষ্ট স্থানে আসার জন্য উৎসাহিত করা হবে। ",
  "PI_POINTER_2" : " ড্রাইভারদের আপনার গতিবিধি সাহায্য করতে উৎসাহিত করা হবে।",
  "VI_POINTER_1" : "ড্রাইভারদেরকে টেক্সট পাঠানোর বদলে কল করার জন্য উৎসাহিত করা হবে। ",
  "VI_POINTER_2" : "ড্রাইভারদেরকে পিকআপে একবার হর্ণ দেওয়ার জন্য উৎসাহিত করা হবে। ",
  "HI_POINTER_1" : "ড্রাইভারদেরকে কল দেওয়ার বদলে টেক্সট পাঠানোর জন্য উৎসাহিত করা হবে। ",
  "HI_POINTER_2" : "ড্রাইভারদেরকে পিকআপে একবার মেসেজ দেওয়ার জন্য উৎসাহিত করা হবে।",
  "ACCESSIBILITY_TEXT" : "নম্মা যাত্রী, এখন আপনার জন্য কাস্টমাইজড!",
  "TO_CATER_YOUR_SPECIFIC_NEEDS" : "আপনার নির্দিষ্ট চাহিদা পূরণের জন্য, আমরা নম্মা যাত্রীর কিছু বৈশিষ্ট্য কাস্টমাইজ করেছি।",
  "SPECIAL_ASSISTANCE" : "বিশেষ সহায়তা",
  "SELECT_THE_CONDITION_THAT_IS_APPLICABLE" : "আপনার জন্য প্রযোজ্য শর্ত নির্বাচন করুন\n(RPWD আইন 2016 অনুযায়ী)",
  "DISABILITY_CLAIMER_TEXT" : "চালিয়ে যাওয়ার মাধ্যমে, আপনি 2016 সালের RPWD আইনের অধীনে প্রতিবন্ধী ব্যক্তি হিসাবে আপনার অবস্থা ঘোষণা করছেন",
  "ARE_YOU_A_PERSON_WITH_DISABILITY": "আপনি কি প্রতিবন্ধী ব্যক্তি?",
  "DO_YOU_NEEED_SPECIAL_ASSISTANCE" : "আপনার কি বিশেষ সহায়তা প্রয়োজন (পার্সন উইথ ডিসেবিলিটি)?",
  "ASSISTANCE_REQUIRED" : "সহায়তা প্রয়োজন (প্রতিবন্ধী ব্যক্তি)",
  "NO_DISABILITY" : "কোন অক্ষমতা নেই",
  "LEARN_HOW_TEXT" : "নম্মা যাত্রী কীভাবে আপনার প্রয়োজন মেটায় তা জানুন",
  "UPDATE_PROFILE" : "প্রফাইল হালনাগাদ",
  "NOW_GET_ASSISTED_RIDES" : "আপনি যদি প্রতিবন্ধী ব্যক্তি হন তবে এখন সহায়ক রাইড পান" ,
  "CLEAN_CAB" : "ক্লিন ক্যাব",
  "SENT_OTP_VIA_WHATSAPP" : "Whatsapp এর মাধ্যমে OTP পাঠানো হয়েছে",
  "SENT_OTP_VIA_SMS" : "Whatsapp এর মাধ্যমে OTP পাঠানো হয়েছে",
  "ENABLE_LOCATION_PERMISSION_TO" : "রাইড অনুসন্ধান করার জন্য অবস্থানের অনুমতি সক্ষম করুন",
  "PLEASE_ENABLE_LOCATION_PERMISSION" : "অনুগ্রহ করে রাইড খোঁজা শুরু করতে সেটিংস অ্যাপ থেকে নম্মা যাত্রীর জন্য অবস্থানের অনুমতি সক্ষম করুন।",
  "PLACES_YOU_MIGHT_LIKE_TO_GO_TO": "সেই জায়গা যেগুলি আপনি পছন্দ করতে পারেন",
  "SUGGESTED_DESTINATION": "প্রস্তুতির গন্তব্য",
  "RECENT_RIDES": "সাম্প্রতিক যাত্রা",
  "ONE_CLICK_BOOKING_FOR_YOUR_FAVOURITE_JOURNEYS": "আপনার পছন্দের যাত্রার জন্য এক ক্লিক বুকিং!",
  "VIEW_MORE": "আরও দেখুন",
  "VIEW_LESS": "কম দেখুন",
  "HAVE_A_REFFERAL" : "আপনার কাছে কোন রেফারেল আছে?",
  "YOUR_SUGGESTED_DESTINATIONS_AND_RECENT_RIDES_WILL_APPEAR_HERE" : "আপনার প্রস্তাবিত গন্তব্য এবং সাম্প্রতিক রাইড এখানে দেখানো হবে",
  "WELCOME_TO_NAMMA_YATRI_" : "নম্মা যাত্রীতে আপনাকে স্বাগতম!",
  "BOOK_AND_MOVE" : "বুক করুন এবং চলুন",
  "ANYWHERE_IN_THE_CITY" : "শহরের যেকোন জায়গায়",
  "CHECKOUT_OUR_LIVE_STATS" : "আমাদের লাইভ পরিসংখ্যান চেক করুন",
  "BENGALURU_MOST_LOVED_APP" : "বেঙ্গালুরুর সবচেয়ে ❤️ অটো অ্যাপ",
  "PICKUP_" : "পিকআপ: ",
}
