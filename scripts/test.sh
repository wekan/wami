#!/bin/bash

# WeKan WAMI Test Script
# Automated testing for FreePascal WeKan implementation

set -e

# Configuration
PROJECT_NAME="wekan"
SOURCE_DIR="src"
BUILD_DIR="build"
TEST_DIR="tests"
COVERAGE_DIR="coverage"
VERSION="1.0.0"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if FreePascal is installed
check_fpc() {
    if ! command -v fpc &> /dev/null; then
        log_error "FreePascal compiler (fpc) is not installed!"
        log_info "Please install FreePascal from https://www.freepascal.org/download.html"
        exit 1
    fi
    
    FPC_VERSION=$(fpc -v | head -n1)
    log_info "Using FreePascal: $FPC_VERSION"
}

# Create test directories
create_directories() {
    log_info "Creating test directories..."
    mkdir -p "$TEST_DIR"
    mkdir -p "$COVERAGE_DIR"
    mkdir -p "$BUILD_DIR"
}

# Run unit tests
run_unit_tests() {
    log_info "Running unit tests..."
    
    # Check if test files exist
    if [ ! -d "$TEST_DIR" ] || [ -z "$(ls -A $TEST_DIR 2>/dev/null)" ]; then
        log_warning "No unit tests found in $TEST_DIR"
        return 0
    fi
    
    local test_count=0
    local passed_count=0
    local failed_count=0
    
    # Find all test files
    for test_file in "$TEST_DIR"/*.pas; do
        if [ -f "$test_file" ]; then
            test_count=$((test_count + 1))
            test_name=$(basename "$test_file" .pas)
            
            log_info "Running test: $test_name"
            
            # Compile test
            if fpc -o"$BUILD_DIR/test_$test_name" "$test_file" 2>/dev/null; then
                # Run test
                if "$BUILD_DIR/test_$test_name" 2>/dev/null; then
                    log_success "Test passed: $test_name"
                    passed_count=$((passed_count + 1))
                else
                    log_error "Test failed: $test_name"
                    failed_count=$((failed_count + 1))
                fi
            else
                log_error "Failed to compile test: $test_name"
                failed_count=$((failed_count + 1))
            fi
        fi
    done
    
    # Summary
    log_info "Unit test summary:"
    log_info "  Total tests: $test_count"
    log_success "  Passed: $passed_count"
    if [ $failed_count -gt 0 ]; then
        log_error "  Failed: $failed_count"
    else
        log_info "  Failed: $failed_count"
    fi
    
    return $failed_count
}

# Run integration tests
run_integration_tests() {
    log_info "Running integration tests..."
    
    # Check if application is built
    if [ ! -f "$BUILD_DIR/$PROJECT_NAME" ]; then
        log_error "Application not built. Run build script first."
        return 1
    fi
    
    # Start application in background
    log_info "Starting application for testing..."
    "$BUILD_DIR/$PROJECT_NAME" &
    APP_PID=$!
    
    # Wait for application to start
    sleep 3
    
    # Test HTTP endpoints
    test_http_endpoints
    
    # Stop application
    log_info "Stopping application..."
    kill $APP_PID 2>/dev/null || true
    wait $APP_PID 2>/dev/null || true
}

# Test HTTP endpoints
test_http_endpoints() {
    log_info "Testing HTTP endpoints..."
    
    local base_url="http://localhost:5500"
    local test_count=0
    local passed_count=0
    local failed_count=0
    
    # Test endpoints
    local endpoints=(
        "/"
        "/login"
        "/register"
        "/boards"
        "/api/boards"
        "/api/users"
    )
    
    for endpoint in "${endpoints[@]}"; do
        test_count=$((test_count + 1))
        log_info "Testing endpoint: $endpoint"
        
        if curl -s -o /dev/null -w "%{http_code}" "$base_url$endpoint" | grep -q "200\|301\|302"; then
            log_success "Endpoint OK: $endpoint"
            passed_count=$((passed_count + 1))
        else
            log_error "Endpoint failed: $endpoint"
            failed_count=$((failed_count + 1))
        fi
    done
    
    # Summary
    log_info "HTTP test summary:"
    log_info "  Total endpoints: $test_count"
    log_success "  Passed: $passed_count"
    if [ $failed_count -gt 0 ]; then
        log_error "  Failed: $failed_count"
    else
        log_info "  Failed: $failed_count"
    fi
    
    return $failed_count
}

# Run performance tests
run_performance_tests() {
    log_info "Running performance tests..."
    
    # Check if application is built
    if [ ! -f "$BUILD_DIR/$PROJECT_NAME" ]; then
        log_error "Application not built. Run build script first."
        return 1
    fi
    
    # Start application in background
    log_info "Starting application for performance testing..."
    "$BUILD_DIR/$PROJECT_NAME" &
    APP_PID=$!
    
    # Wait for application to start
    sleep 3
    
    # Test startup time
    test_startup_time
    
    # Test memory usage
    test_memory_usage
    
    # Test response times
    test_response_times
    
    # Stop application
    log_info "Stopping application..."
    kill $APP_PID 2>/dev/null || true
    wait $APP_PID 2>/dev/null || true
}

# Test startup time
test_startup_time() {
    log_info "Testing startup time..."
    
    local start_time=$(date +%s%N)
    "$BUILD_DIR/$PROJECT_NAME" &
    local app_pid=$!
    
    # Wait for application to start (check if port is open)
    local max_wait=10
    local wait_count=0
    while [ $wait_count -lt $max_wait ]; do
        if curl -s http://localhost:5500 > /dev/null 2>&1; then
            break
        fi
        sleep 1
        wait_count=$((wait_count + 1))
    done
    
    local end_time=$(date +%s%N)
    local startup_time=$(( (end_time - start_time) / 1000000 )) # Convert to milliseconds
    
    log_info "Startup time: ${startup_time}ms"
    
    # Stop application
    kill $app_pid 2>/dev/null || true
    wait $app_pid 2>/dev/null || true
    
    if [ $startup_time -lt 1000 ]; then
        log_success "Startup time is excellent (< 1s)"
    elif [ $startup_time -lt 3000 ]; then
        log_success "Startup time is good (< 3s)"
    else
        log_warning "Startup time is slow (> 3s)"
    fi
}

# Test memory usage
test_memory_usage() {
    log_info "Testing memory usage..."
    
    # Start application
    "$BUILD_DIR/$PROJECT_NAME" &
    local app_pid=$!
    
    # Wait for application to start
    sleep 3
    
    # Get memory usage
    local memory_kb=$(ps -o rss= -p $app_pid 2>/dev/null || echo "0")
    local memory_mb=$((memory_kb / 1024))
    
    log_info "Memory usage: ${memory_mb}MB"
    
    # Stop application
    kill $app_pid 2>/dev/null || true
    wait $app_pid 2>/dev/null || true
    
    if [ $memory_mb -lt 50 ]; then
        log_success "Memory usage is excellent (< 50MB)"
    elif [ $memory_mb -lt 100 ]; then
        log_success "Memory usage is good (< 100MB)"
    else
        log_warning "Memory usage is high (> 100MB)"
    fi
}

# Test response times
test_response_times() {
    log_info "Testing response times..."
    
    local base_url="http://localhost:5500"
    local total_time=0
    local request_count=10
    
    for i in $(seq 1 $request_count); do
        local start_time=$(date +%s%N)
        curl -s "$base_url/" > /dev/null
        local end_time=$(date +%s%N)
        local response_time=$(( (end_time - start_time) / 1000000 )) # Convert to milliseconds
        total_time=$((total_time + response_time))
    done
    
    local avg_response_time=$((total_time / request_count))
    log_info "Average response time: ${avg_response_time}ms"
    
    if [ $avg_response_time -lt 50 ]; then
        log_success "Response time is excellent (< 50ms)"
    elif [ $avg_response_time -lt 100 ]; then
        log_success "Response time is good (< 100ms)"
    else
        log_warning "Response time is slow (> 100ms)"
    fi
}

# Run security tests
run_security_tests() {
    log_info "Running security tests..."
    
    # Check for common security issues
    local security_issues=0
    
    # Check for hardcoded passwords
    if grep -r "password.*=" "$SOURCE_DIR" 2>/dev/null | grep -v "// TODO\|// FIXME" | grep -q "admin\|123\|password"; then
        log_warning "Potential hardcoded passwords found"
        security_issues=$((security_issues + 1))
    fi
    
    # Check for SQL injection vulnerabilities
    if grep -r "SELECT.*+" "$SOURCE_DIR" 2>/dev/null | grep -v "// TODO\|// FIXME" | grep -q "\+.*\$"; then
        log_warning "Potential SQL injection vulnerabilities found"
        security_issues=$((security_issues + 1))
    fi
    
    # Check for XSS vulnerabilities
    if grep -r "innerHTML\|document.write" "$SOURCE_DIR" 2>/dev/null | grep -v "// TODO\|// FIXME"; then
        log_warning "Potential XSS vulnerabilities found"
        security_issues=$((security_issues + 1))
    fi
    
    if [ $security_issues -eq 0 ]; then
        log_success "No obvious security issues found"
    else
        log_warning "Found $security_issues potential security issues"
    fi
    
    return $security_issues
}

# Run code quality tests
run_code_quality_tests() {
    log_info "Running code quality tests..."
    
    local quality_issues=0
    
    # Check for TODO/FIXME comments
    local todo_count=$(grep -r "TODO\|FIXME" "$SOURCE_DIR" 2>/dev/null | wc -l)
    if [ $todo_count -gt 0 ]; then
        log_info "Found $todo_count TODO/FIXME comments"
    fi
    
    # Check for long lines (> 120 characters)
    local long_lines=$(grep -r "^.\{121,\}" "$SOURCE_DIR" 2>/dev/null | wc -l)
    if [ $long_lines -gt 0 ]; then
        log_warning "Found $long_lines lines longer than 120 characters"
        quality_issues=$((quality_issues + 1))
    fi
    
    # Check for unused variables (basic check)
    if grep -r "var.*:.*;" "$SOURCE_DIR" 2>/dev/null | grep -q "unused\|temp\|tmp"; then
        log_warning "Potential unused variables found"
        quality_issues=$((quality_issues + 1))
    fi
    
    if [ $quality_issues -eq 0 ]; then
        log_success "Code quality looks good"
    else
        log_warning "Found $quality_issues code quality issues"
    fi
    
    return $quality_issues
}

# Generate test report
generate_test_report() {
    log_info "Generating test report..."
    
    local report_file="test-report-$(date +%Y%m%d-%H%M%S).txt"
    
    {
        echo "WeKan WAMI Test Report"
        echo "Generated: $(date)"
        echo "Version: $VERSION"
        echo "=================================="
        echo ""
        echo "Test Summary:"
        echo "- Unit Tests: $([ $? -eq 0 ] && echo "PASSED" || echo "FAILED")"
        echo "- Integration Tests: $([ $? -eq 0 ] && echo "PASSED" || echo "FAILED")"
        echo "- Performance Tests: $([ $? -eq 0 ] && echo "PASSED" || echo "FAILED")"
        echo "- Security Tests: $([ $? -eq 0 ] && echo "PASSED" || echo "FAILED")"
        echo "- Code Quality Tests: $([ $? -eq 0 ] && echo "PASSED" || echo "FAILED")"
        echo ""
        echo "For detailed results, check the console output above."
    } > "$report_file"
    
    log_success "Test report generated: $report_file"
}

# Clean test artifacts
clean_tests() {
    log_info "Cleaning test artifacts..."
    rm -rf "$BUILD_DIR/test_*"
    rm -f test-report-*.txt
    log_success "Test artifacts cleaned"
}

# Show help
show_help() {
    echo "WeKan WAMI Test Script"
    echo ""
    echo "Usage: $0 [OPTIONS] [TEST_TYPES]"
    echo ""
    echo "Options:"
    echo "  -h, --help     Show this help message"
    echo "  -c, --clean    Clean test artifacts"
    echo "  -a, --all      Run all tests"
    echo "  -r, --report   Generate test report"
    echo ""
    echo "Test Types:"
    echo "  unit           Unit tests"
    echo "  integration    Integration tests"
    echo "  performance    Performance tests"
    echo "  security       Security tests"
    echo "  quality        Code quality tests"
    echo ""
    echo "Examples:"
    echo "  $0 unit                    # Run unit tests only"
    echo "  $0 unit integration        # Run unit and integration tests"
    echo "  $0 --all                   # Run all tests"
    echo "  $0 --clean                 # Clean test artifacts"
}

# Main function
main() {
    local test_types=()
    local run_all=false
    local clean_tests_flag=false
    local generate_report=false
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -c|--clean)
                clean_tests_flag=true
                shift
                ;;
            -a|--all)
                run_all=true
                shift
                ;;
            -r|--report)
                generate_report=true
                shift
                ;;
            -*)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
            *)
                test_types+=("$1")
                shift
                ;;
        esac
    done
    
    # Handle clean option
    if [ "$clean_tests_flag" = true ]; then
        clean_tests
        exit 0
    fi
    
    # Check FreePascal installation
    check_fpc
    
    # Create test directories
    create_directories
    
    # Determine test types to run
    if [ "$run_all" = true ]; then
        test_types=("unit" "integration" "performance" "security" "quality")
    elif [ ${#test_types[@]} -eq 0 ]; then
        test_types=("unit" "integration")
    fi
    
    local total_failures=0
    
    # Run selected tests
    for test_type in "${test_types[@]}"; do
        case $test_type in
            "unit")
                run_unit_tests
                total_failures=$((total_failures + $?))
                ;;
            "integration")
                run_integration_tests
                total_failures=$((total_failures + $?))
                ;;
            "performance")
                run_performance_tests
                total_failures=$((total_failures + $?))
                ;;
            "security")
                run_security_tests
                total_failures=$((total_failures + $?))
                ;;
            "quality")
                run_code_quality_tests
                total_failures=$((total_failures + $?))
                ;;
            *)
                log_error "Unknown test type: $test_type"
                total_failures=$((total_failures + 1))
                ;;
        esac
    done
    
    # Generate report if requested
    if [ "$generate_report" = true ]; then
        generate_test_report
    fi
    
    # Final summary
    if [ $total_failures -eq 0 ]; then
        log_success "All tests completed successfully!"
    else
        log_error "Some tests failed. Total failures: $total_failures"
    fi
    
    exit $total_failures
}

# Run main function
main "$@"
