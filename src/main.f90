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
    real                    :: rValue
    real, dimension(16384)  :: rvPrimarySet
    real, dimension(16384)  :: rvSecondarySet
    type(CompareType)       :: tCmp
    real                    :: rFB
    
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
        end if
        
        iRetCode = tCmp % Set(rvPrimarySet, rvSecondarySet)
        if(iRetCode /= 0) then
            print *, "error: Comparison data sets initialization failure - Ret code = ", iRetCode
            stop
        end if
        
        rFB = tCmp % FB()
        print *, "    Fractional Bias (FB) = ", rFB
        
    end do

end program GenerateIndicators

