!check_field_dependency.f90
!     module check_field_dependency
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!      subroutine check_dependence_phys(num_nod_phys, num_check,        &
!     &   target_name, phys_nod_name, phys_check_name)
!
      module check_field_dependency
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependence_phys(num_nod_phys, num_check,         &
     &   target_name, phys_nod_name, phys_check_name)
!
      use m_parallel_var_dof
!
      integer(kind=kint) :: num_nod_phys, num_check
      character(len=kchara) :: target_name
      character(len=kchara) :: phys_nod_name(num_nod_phys)
      character(len=kchara) :: phys_check_name(num_check)
!
      integer(kind=kint) :: iflag, j, jj
!
!
      iflag = 0
!
      do j = 1, num_nod_phys
        do jj = 1, num_check
          if (  phys_nod_name(j) .eq. phys_check_name(jj)  ) then
            iflag = iflag + 1
          end if
        end do
      end do
!
      if (iflag .lt. num_check) then
        if (my_rank.eq.0) then 
          write(*,*) 'Following parameters are required for ',          &
     &                trim(target_name)
          do j = 1, num_check
            write(*,*)  trim(phys_check_name(j))
          end do
        end if
        call parallel_abort(402,'Stop program.')
      end if
!
      end subroutine check_dependence_phys
!
! -----------------------------------------------------------------------
!
      end module check_field_dependency
