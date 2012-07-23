//
//  ObjectiveHaskellTests.m
//  ObjectiveHaskellTests
//
//  Created by Justin Spahr-Summers on 2012-07-13.
//  Released into the public domain.
//

#import "ObjectiveHaskellTests.h"
#import "BridgedObjectTest_stub.h"
#import "FibTest_stub.h"
#import "MsgSendTest_stub.h"

@implementation ObjectiveHaskellTests

- (void)testNSStringBridging {
    NSString *str = appendFoobar(@"fuzzbuzz");
    STAssertEqualObjects(str, @"fuzzbuzzfoobar", @"");
}

- (void)testNSArrayBridging {
    NSArray *array = addFoobarToArray(@[ @5, @{} ]);
    NSArray *expectedArray = @[ @5, @{}, @"foobar" ];
    STAssertEqualObjects(array, expectedArray, @"");
}

- (void)testNSDataBridging {
    unsigned char bytes[] = { 1, 2, 3 };
    NSData *data = appendByte([NSData dataWithBytes:bytes length:sizeof(bytes)], 5);
    
    unsigned char expectedBytes[] = { 1, 2, 3, 5 };
    STAssertEquals([data length], sizeof(expectedBytes), @"");
    STAssertTrue(memcmp([data bytes], expectedBytes, sizeof(expectedBytes)) == 0, @"");
}

- (void)testNSDictionaryBridging {
    NSDictionary *dict = setFooToBar(@{ @"fuzz": @5, @"foo": @"buzz" });
    NSDictionary *expectedDict = @{ @"fuzz": @5, @"foo": @"bar" };
    STAssertEqualObjects(dict, expectedDict, @"");
}

- (void)testNSNumberBridging {
	NSNumber *inum = @5;
	STAssertEqualObjects(@6, plusInt(inum, 1), @"");
	STAssertEqualsWithAccuracy(6.5, [plusDouble(inum, 1.5) doubleValue], 0.001, @"");

	NSNumber *dnum = @5.25;
	STAssertEqualsWithAccuracy(6.25, [plusInt(dnum, 1) doubleValue], 0.001, @"");
	STAssertEqualsWithAccuracy(6.75, [plusDouble(dnum, 1.5) doubleValue], 0.001, @"");
}

- (void)testNSValueBridging {
	NSValue *value = nullNSValue();
	STAssertNotNil(value, @"");
	STAssertNil(value.pointerValue, @"");

	uintptr_t addr = ptrAddress(value);
	STAssertEquals(addr, (uintptr_t)0, @"");
}

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
