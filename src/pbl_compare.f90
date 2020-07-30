! The pbl-compare library, and Fortran module.
!
! This is open source software, covered by the MIT license.
!
! Author: Patrizia Favaron


module pbl_compare

    implicit none
    
    private
    
    ! Public interface
    public  :: CompareType
    
    ! Main data type
    type CompareType
        logical                         :: lGo = .false.
        integer                         :: iNumData
        real, dimension(:), allocatable :: rvPrimary
        real, dimension(:), allocatable :: rvSecondary
    contains
        procedure   :: Set
        procedure   :: FB       ! Fractional bias
    end type CompareType
    
contains

    function Set(this, rvP, rvS) result(iRetCode)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real, dimension(:), intent(in)      :: rvP
        real, dimension(:), intent(in)      :: rvS
        integer                             :: iRetCode
        
        ! Locals
        integer     :: iErrCode
        integer     :: iSize
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check the input data is "correct", that is,
        ! the two sets are non-empty and of same size
        iSize = size(rvP)
        if(iSize <= 0) then
            iRetCode = 1
            return
        end if
        if(iSize /= size(rvS)) then
            iRetCode = 2
            return
        end if
        ! Note the two tests, apparently not covering the whole intention,
        ! in reality test it exhaistively.
        
        ! Clean-out and fill internal space
        if(allocated(this % rvPrimary))   deallocate(this % rvPrimary)
        if(allocated(this % rvSecondary)) deallocate(this % rvSecondary)
        allocate(this % rvPrimary(iSize))
        allocate(this % rvSecondary(iSize))
        this % rvPrimary   = rvP
        this % rvSecondary = rvS
        
        ! Indicate function computing may really start
        this % lGo = .true.
        
    end function Set
    
    
    ! Fractional Bias (FB)
    function FB(this) result(rFB)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real                                :: rFB
        
        ! Locals
        real    :: rNumerator
        real    :: rDenominator
        
        ! Check execution may start
        if(.not.this % lGo) then
            rFb = -9999.9
            return
        end if
        
        ! Compute the information desired
        rNumerator   = 2. * sum(this % rvPrimary - this % rvSecondary)
        rDenominator = (sum(this % rvPrimary + this % rvSecondary))
        rFB          = rNumerator / rDenominator
        
    end function FB

end module pbl_compare
