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
        real, dimension(:), allocatable :: rvPrimary
        real, dimension(:), allocatable :: rvSecondary
    contains
        procedure   :: Set
    end type CompareType
    
contains

    function Set(this, rvP, rvS) result(iRetCode)
    
        ! Routine arguments
        class(pbl_compare), intent(inout)   :: this
        real, dimension(:), intent(in)      :: rvP
        real, dimension(:), intent(in)      :: rvS
        integer                             :: iRetCode
        
        ! Locals
        integer     :: iErrCode
        integer     :: iSize
        
    end function Set

end module pbl_compare
