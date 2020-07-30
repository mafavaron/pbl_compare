! Evaluation of standard validation indicators on some types of differences along a transect.
!
! This program is part of the pbl-compare project, and is covered by the MIT license.
!
! Author: Patrizia Favaron

program GenerateIndicators

    implicit none

    ! Locals
    integer                 :: iVariant
    real                    :: rValue
    real, dimension(16384)  :: rvPrimarySet
    real, dimension(16384)  :: rvSecondarySet
    
    ! First case: Single peak of growing size
    rvPrimarySet   = 1.
    rvSecondarySet = rvPrimarySet
    
    do iVariant = 0, 6
        rValue = iVariant * 100.
        rvSecondarySet(8192) = rValue
    end do

end program GenerateIndicators

