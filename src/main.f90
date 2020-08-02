! Evaluation of standard validation indicators on some types of differences along a transect.
!
! This program is part of the pbl-compare project, and is covered by the MIT license.
!
! Author: Patrizia Favaron

program GenerateIndicators

    use pbl_compare

    implicit none

    ! Locals
    integer                 :: iRetCode
    integer                 :: iVariant
    integer                 :: iShiftVal
    real                    :: rValue
    real, dimension(16384)  :: rvPrimarySet
    real, dimension(16384)  :: rvSecondarySet
    type(CompareType)       :: tCmp
    real                    :: rFB
    real                    :: rDs
    integer                 :: n = size(rvPrimarySet)
    integer                 :: i
    
    ! First case: Single peak of growing size
    rvPrimarySet   = 1.
    rvSecondarySet = rvPrimarySet
    
    print *, "   "
    print *, "---   ---   ---"
    print *, "   "
    
    do iVariant = 0, 6
    
        print *, "Action: 'Peaks'; Variant = ", iVariant
        
        if(iVariant > 0) then
            rValue = (iVariant + 1) * 100.
            rvSecondarySet(8192) = rValue
            rDs                  = rValue
        else
            rDs = 1.
        end if
        
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        if(iRetCode /= 0) then
            print *, "error: Comparison data sets initialization failure - Ret code = ", iRetCode
            stop
        end if
        
        ! Compute value expected analytically, for comparison
        rValue = 2. * (1. - rDs) / (rDs + 2*n - 1.)
        
        rFB = tCmp % FB()
        print *, "    Fractional Bias (FB) = ", rFB, "    Error = ", rFB - rValue
        
    end do
    
    print *, "---------------------------------------------------"
        
    rvPrimarySet = [(exp(-(i-n/2)**2/2.e6),i=1,n)]
    do iShiftVal = -6000, 6000, 100
        rvPrimarySet = [(exp(-(i-n/2+iShiftVal)**2/2.e6),i=1,n)]
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        rFB = tCmp % FB()
        print *, "    Fractional Bias (FB) = ", rFB
    end do

end program GenerateIndicators

