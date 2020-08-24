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
        logical                             :: lGo = .false.
        integer                             :: iNumData
        real, dimension(:), allocatable     :: rvPrimary
        real, dimension(:), allocatable     :: rvSecondary
    contains
        procedure   :: Set
        procedure   :: FB       ! Fractional bias
        procedure   :: NMSE     ! Normalized Mean Squared Error
        procedure   :: MG       ! Geometric mean
        procedure   :: VG       ! Geometric variance
        procedure   :: FAC2     ! Fracion within a factor of 2
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
        
        ! Check data is not identically zero
        if(sum(rvP) > epsilon(rvP(1))*iSize .and. sum(rvS) > epsilon(rvS(1))*iSize) then
        
            ! Set data set size
            this % iNumData = iSize
                    
            ! Indicate function computing may really start
            this % lGo = .true.
        
        end if
        
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
            rFB = -9999.9
            return
        end if
        
        ! Compute the information desired
        rNumerator   = 2. * sum(this % rvPrimary - this % rvSecondary)
        rDenominator = sum(this % rvPrimary) + sum(this % rvSecondary)
        rFB          = rNumerator / rDenominator
        
    end function FB

    ! Normalized Mean Squared Error (NMSE)
    function NMSE(this) result(rNMSE)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real                                :: rNMSE
        
        ! Locals
        real    :: rNumerator
        real    :: rDenominator
        
        ! Check execution may start
        if(.not.this % lGo) then
            rNMSE = -9999.9
            return
        end if
        
        ! Compute the information desired
        rNumerator   = sum((this % rvPrimary - this % rvSecondary)**2)
        rDenominator = sum(this % rvPrimary) * sum(this % rvSecondary)
        rNMSE        = this % iNumData * rNumerator / rDenominator
        
    end function NMSE

    ! Geometric Mean (GM)
    function MG(this) result(rMG)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real                                :: rMG
        
        ! Locals
        real    :: rPos
        real    :: rNeg
        integer :: i
        
        ! Check execution may start
        if(.not.this % lGo) then
            rMG = -9999.9
            return
        end if
        
        ! Compute the information desired
        rPos = 0.
        rNeg = 0.
        do i = 1, this % iNumData
            if(this % rvPrimary(i) > 0. .and. this % rvSecondary(i) > 0.) then
                rPos = rPos + log(this % rvPrimary(i))
                rNeg = rNeg + log(this % rvSecondary(i))
            end if
        end do
        if(abs(rPos) < epsilon(rPos) .and. abs(rNeg) < epsilon(rNeg)) then
            rMG = -9999.9
        else
            rMG = exp(rPos - rNeg)
        end if
        
    end function MG

    ! Geometric Variance (VG)
    function VG(this) result(rVG)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real                                :: rVG
        
        ! Locals
        real    :: rPos
        integer :: i
        integer :: iPositive
        
        ! Check execution may start
        if(.not.this % lGo) then
            rVG = -9999.9
            return
        end if
        
        ! Compute the information desired
        rPos = 0.
        iPositive = 0
        do i = 1, this % iNumData
            if(this % rvPrimary(i) > 0. .and. this % rvSecondary(i) > 0.) then
                rPos = rPos + (log(this % rvPrimary(i)) - log(this % rvSecondary(i)))**2
                iPositive = iPositive + 1
            end if
        end do
        rVG = exp(rPos / iPositive)
        
    end function VG

    ! Fraction within a factor of 2 (FAC2)
    function FAC2(this) result(rFAC2)
    
        ! Routine arguments
        class(CompareType), intent(inout)   :: this
        real                                :: rFAC2
        
        ! Locals
        integer :: i
        integer :: iWithin
        real    :: rLower
        real    :: rUpper
        
        ! Check execution may start
        if(.not.this % lGo) then
            rFAC2 = -9999.9
            return
        end if
        
        ! Compute the information desired
        iWithin = 0
        do i = 1, this % iNumData
            if(0.5 * this % rvSecondary(i) <= this % rvPrimary(i) .and. this % rvPrimary(i) <= 2. * this % rvSecondary(i)) then
                iWithin = iWithin + 1
            end if
        end do
        rFAC2 = float(iWithin) / this % iNumData
        
    end function FAC2

end module pbl_compare
