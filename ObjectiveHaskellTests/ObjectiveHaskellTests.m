//
//  ObjectiveHaskellTests.m
//  ObjectiveHaskellTests
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#import "ObjectiveHaskellTests.h"
#import "FibTest_stub.h"
#import "MsgSendTest_stub.h"

@implementation ObjectiveHaskellTests

- (void)testFibonacci {
    STAssertEquals(3, fibonacci_hs(4), @"");
    STAssertEquals(5, fibonacci_hs(5), @"");
}

- (void)testMsgSend {
    NSString *str = @"foo";
    NSMutableString *mutableStr = msgSendTest(str);

    STAssertEqualObjects(str, mutableStr, @"");
    STAssertFalse(str == mutableStr, @"");
}

- (void)testMsgSendMemoryManagement {
    // used to test the memory management of the string returned from Haskell
    __weak id weakStr = nil;

    @autoreleasepool {
        {
            __attribute__((objc_precise_lifetime)) NSMutableString *str = msgSendTest(@"foo");

            // not necessary for normal code -- just for memory management testing
            hs_perform_gc();

            weakStr = str;
            STAssertNotNil(weakStr, @"");

            STAssertEqualObjects(str, @"foo", @"");
        }

        // the string shouldn't be released until the autorelease pool is popped
        STAssertNotNil(weakStr, @"");
    }

    STAssertNil(weakStr, @"");
}

@end
