!m_element_group.f90
!     module m_element_group
!
!> @brief element group data
!
!     Written by H. Matsui
!
!      subroutine check_mat_4_sheard_para(my_rank)
!
      module m_element_group
!
      use m_precision
      use t_group_data
!
      implicit  none
!
!
!>  Structure for element group
      type(group_data), save :: ele_grp1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_mat_4_sheard_para(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'num_mat ', ele_grp1%num_grp
       write(*,*) 'PE: ', my_rank, 'num_mat_smp ', ele_grp1%num_grp_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &            'imat_smp_stack ', ele_grp1%istack_grp_smp
!
      end subroutine check_mat_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_element_group
