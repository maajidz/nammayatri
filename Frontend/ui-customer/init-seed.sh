# npm run start

# To enable this command: Tools -> Create command line launcher -> {Add suggested path} and save

studio ../android-native

osascript -e 'tell app "Terminal"
    do script "~/Library/Android/sdk/tools/emulator @pixel"
end tell'

cd ../android-native
./gradlew tasks
# ./gradlew assemble
