!m_element_group.f90
!     module m_element_group
!
!> @brief element group data
!
!     Written by H. Matsui
!
!      subroutine allocate_material_data
!      subroutine deallocate_material_data
!
!      subroutine clear_material_data
!
!       subroutine allocate_material_param_smp
!       subroutine deallocate_material_param_smp
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
!>  Structure for node and element group
      type(group_data), save :: ele_grp1
!
!ele_grp1%istack_grp
!
!      integer (kind=kint) :: num_mat
!<      number of element group
!      integer (kind=kint) :: num_mat_bc
!<      total number of nodes for element group
! 
!      integer (kind=kint), allocatable, target :: mat_istack(:)
!<      end address of each element group
      integer (kind=kint), allocatable, target :: mat_item(:)
!<      local element ID for element group
! 
!      character (len=kchara), allocatable, target :: mat_name(:)
!<      element group name
!
!      integer( kind=kint )  ::  num_mat_smp
!<      number of element group for SMP process
      integer( kind=kint ), allocatable :: imat_smp_stack(:)
!<      end address of each element group for SMP process
!
!      integer( kind=kint )  ::  max_mat_4_smp
!<      maximum number of element group for SMP process
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_material_data
!
       allocate(ele_grp1%istack_grp(0:ele_grp1%num_grp))
       allocate(ele_grp1%grp_name(ele_grp1%num_grp))
       allocate(mat_item(ele_grp1%num_item))
!
      call clear_material_data
!
      end subroutine allocate_material_data
!
! ----------------------------------------------------------------------
!
      subroutine clear_material_data
!
      ele_grp1%istack_grp = 0
      if(ele_grp1%num_item .gt. 0) mat_item = 0
!
      end subroutine clear_material_data
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_material_data
!
       deallocate(ele_grp1%istack_grp)
       deallocate(ele_grp1%grp_name)
       deallocate(mat_item)
!
      end subroutine deallocate_material_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine allocate_material_param_smp
!
       allocate( imat_smp_stack(0:ele_grp1%num_grp_smp))
       imat_smp_stack = 0
!
       end subroutine allocate_material_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_material_param_smp
!
       deallocate(imat_smp_stack)
!
       end subroutine deallocate_material_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_mat_4_sheard_para(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank, 'num_mat ', ele_grp1%num_grp
       write(*,*) 'PE: ', my_rank, 'num_mat_smp ', ele_grp1%num_grp_smp
       write(*,*) 'PE: ', my_rank,                                      &
     &            'imat_smp_stack ', imat_smp_stack
!
      end subroutine check_mat_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_element_group
