#!/bin/bash

# WeKan WAMI Build Script
# FreePascal compilation script for cross-platform builds

set -e

# Configuration
PROJECT_NAME="wekan"
SOURCE_DIR="src"
BUILD_DIR="build"
DIST_DIR="dist"
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

# Create build directories
create_directories() {
    log_info "Creating build directories..."
    mkdir -p "$BUILD_DIR"
    mkdir -p "$DIST_DIR"
    mkdir -p "uploads"
    mkdir -p "backups"
    mkdir -p "logs"
}

# Compile for specific platform
compile_platform() {
    local platform=$1
    local target=$2
    local arch=$3
    
    log_info "Compiling for $platform ($target-$arch)..."
    
    case $platform in
        "linux")
            fpc -Tlinux -Px86_64 -O3 -Xs -XX -Fu"$SOURCE_DIR/web" -Fu"$SOURCE_DIR/auth" -Fu"$SOURCE_DIR/utils" -o"$BUILD_DIR/${PROJECT_NAME}-linux-x64" "$SOURCE_DIR/wekan.pas"
            ;;
        "windows")
            fpc -Twindows -Px86_64 -O3 -Xs -XX -Fu"$SOURCE_DIR/web" -Fu"$SOURCE_DIR/auth" -Fu"$SOURCE_DIR/utils" -o"$BUILD_DIR/${PROJECT_NAME}-windows-x64.exe" "$SOURCE_DIR/wekan.pas"
            ;;
        "darwin")
            fpc -Tdarwin -Px86_64 -O3 -Xs -XX -Fu"$SOURCE_DIR/web" -Fu"$SOURCE_DIR/auth" -Fu"$SOURCE_DIR/utils" -o"$BUILD_DIR/${PROJECT_NAME}-macos-x64" "$SOURCE_DIR/wekan.pas"
            ;;
        "arm64")
            fpc -Tlinux -Parm -O3 -Xs -XX -Fu"$SOURCE_DIR/web" -Fu"$SOURCE_DIR/auth" -Fu"$SOURCE_DIR/utils" -o"$BUILD_DIR/${PROJECT_NAME}-linux-arm64" "$SOURCE_DIR/wekan.pas"
            ;;
        "m68k")
            fpc -Tlinux -Pm68k -O3 -Xs -XX -Fu"$SOURCE_DIR/web" -Fu"$SOURCE_DIR/auth" -Fu"$SOURCE_DIR/utils" -o"$BUILD_DIR/${PROJECT_NAME}-linux-m68k" "$SOURCE_DIR/wekan.pas"
            ;;
        *)
            log_error "Unknown platform: $platform"
            return 1
            ;;
    esac
    
    if [ $? -eq 0 ]; then
        log_success "Successfully compiled for $platform"
    else
        log_error "Failed to compile for $platform"
        return 1
    fi
}

# Copy static files
copy_static_files() {
    log_info "Copying static files..."
    
    # Copy public directory
    if [ -d "public" ]; then
        cp -r public "$DIST_DIR/"
        log_success "Copied public directory"
    fi
    
    # Copy configuration
    if [ -f "config/wekan.conf" ]; then
        cp config/wekan.conf "$DIST_DIR/"
        log_success "Copied configuration file"
    fi
    
    # Copy documentation
    if [ -f "README.md" ]; then
        cp README.md "$DIST_DIR/"
        log_success "Copied README"
    fi
}

# Create distribution packages
create_packages() {
    log_info "Creating distribution packages..."
    
    # Create tar.gz packages
    for binary in "$BUILD_DIR"/*; do
        if [ -f "$binary" ]; then
            platform=$(basename "$binary" | cut -d'-' -f2-3)
            package_name="${PROJECT_NAME}-${VERSION}-${platform}"
            
            mkdir -p "$DIST_DIR/$package_name"
            cp "$binary" "$DIST_DIR/$package_name/"
            cp -r "$DIST_DIR/public" "$DIST_DIR/$package_name/" 2>/dev/null || true
            cp "$DIST_DIR/wekan.conf" "$DIST_DIR/$package_name/" 2>/dev/null || true
            cp "$DIST_DIR/README.md" "$DIST_DIR/$package_name/" 2>/dev/null || true
            
            cd "$DIST_DIR"
            tar -czf "${package_name}.tar.gz" "$package_name"
            cd ..
            
            log_success "Created package: ${package_name}.tar.gz"
        fi
    done
}

# Clean build artifacts
clean() {
    log_info "Cleaning build artifacts..."
    rm -rf "$BUILD_DIR"
    rm -rf "$DIST_DIR"
    log_success "Cleaned build artifacts"
}

# Show help
show_help() {
    echo "WeKan WAMI Build Script"
    echo ""
    echo "Usage: $0 [OPTIONS] [PLATFORMS]"
    echo ""
    echo "Options:"
    echo "  -h, --help     Show this help message"
    echo "  -c, --clean    Clean build artifacts"
    echo "  -a, --all      Build for all platforms"
    echo "  -v, --version  Show version"
    echo ""
    echo "Platforms:"
    echo "  linux          Linux x86_64"
    echo "  windows        Windows x86_64"
    echo "  darwin         macOS x86_64"
    echo "  arm64          Linux ARM64"
    echo "  m68k           Linux m68k (Amiga)"
    echo ""
    echo "Examples:"
    echo "  $0 linux                    # Build for Linux only"
    echo "  $0 linux windows            # Build for Linux and Windows"
    echo "  $0 --all                    # Build for all platforms"
    echo "  $0 --clean                  # Clean build artifacts"
}

# Main function
main() {
    local platforms=()
    local build_all=false
    local clean_build=false
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -c|--clean)
                clean_build=true
                shift
                ;;
            -a|--all)
                build_all=true
                shift
                ;;
            -v|--version)
                echo "WeKan WAMI v$VERSION"
                exit 0
                ;;
            -*)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
            *)
                platforms+=("$1")
                shift
                ;;
        esac
    done
    
    # Handle clean option
    if [ "$clean_build" = true ]; then
        clean
        exit 0
    fi
    
    # Check FreePascal installation
    check_fpc
    
    # Create build directories
    create_directories
    
    # Determine platforms to build
    if [ "$build_all" = true ]; then
        platforms=("linux" "windows" "darwin" "arm64" "m68k")
    elif [ ${#platforms[@]} -eq 0 ]; then
        # Default to current platform
        case "$(uname -s)" in
            Linux*)
                platforms=("linux")
                ;;
            Darwin*)
                platforms=("darwin")
                ;;
            CYGWIN*|MINGW32*|MSYS*)
                platforms=("windows")
                ;;
            *)
                platforms=("linux")
                ;;
        esac
    fi
    
    # Build for each platform
    for platform in "${platforms[@]}"; do
        compile_platform "$platform"
    done
    
    # Copy static files
    copy_static_files
    
    # Create distribution packages
    create_packages
    
    log_success "Build completed successfully!"
    log_info "Binaries are available in: $BUILD_DIR/"
    log_info "Packages are available in: $DIST_DIR/"
}

# Run main function
main "$@"
