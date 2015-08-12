!set_element_refine_flag.f90
!      module set_element_refine_flag
!
!      Written by Kemorin on Oct., 2007
!
!      subroutine set_element_refine_flag(numele, ele_grp)
!
      module set_element_refine_flag
!
      use m_precision
!
      use m_control_param_4_refiner
!
      implicit    none
!
!
      integer(kind = kint) :: nele_tri
      integer(kind = kint), allocatable :: iele_tri(:)
!
      private :: set_refine_flag_by_ele_grp
      private :: count_triple_refine_table, set_triple_refine_table
      private :: const_triple_refine_table
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_element_refine_flag(numele, ele_grp)
!
      use t_group_data
      use find_boundary_4_tri_refine
!
      integer(kind = kint), intent(in) :: numele
      type(group_data), intent(in) :: ele_grp
!
!
      call set_refine_flag_by_ele_grp(numele, ele_grp)
      if (id_refined_ele_grp(1) .eq. -1) return
!
!
      call const_triple_refine_table
!
      write(*,*) 's_find_boundary_4_tri_refine'
      call s_find_boundary_4_tri_refine(nele_tri, iele_tri)
!
      deallocate(iele_tri)
!
      end subroutine s_set_element_refine_flag
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_refine_flag_by_ele_grp(numele, ele_grp)
!
      use m_refined_element_data
      use t_group_data
!
      integer(kind = kint), intent(in) :: numele
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: j, i, ist, ied, inum, iele
!
!
      if (id_refined_ele_grp(1) .eq. -1) then
!
        do iele = 1, numele
          iflag_refine_ele(iele) = iflag_refine_type(1)
        end do
!
      else
!
        do j = 1,  num_refine_type
          i = id_refined_ele_grp(j)
          ist = ele_grp%istack_grp(i-1) + 1
          ied = ele_grp%istack_grp(i)
          do inum = ist, ied
            iele = ele_grp%item_grp(inum)
            iflag_refine_ele(iele) = iflag_refine_type(j)
          end do
        end do
!
      end if
!
      end subroutine set_refine_flag_by_ele_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_triple_refine_table
!
!
      call count_triple_refine_table
!
      allocate(iele_tri(nele_tri))
      iele_tri = 0
!
      call set_triple_refine_table
!
      end subroutine const_triple_refine_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_triple_refine_table
!
      use m_geometry_data
      use m_refined_element_data
      use m_refine_flag_parameters
!
      integer(kind = kint) :: iele
!
!
      nele_tri = 0
      do iele = 1, ele1%numele
        if(iflag_refine_ele(iele) .eq. iflag_tri_full) then
          nele_tri = nele_tri + 1
        end if
      end do
!
      end subroutine count_triple_refine_table
!
! -----------------------------------------------------------------------
!
      subroutine set_triple_refine_table
!
      use m_geometry_data
      use m_refined_element_data
      use m_refine_flag_parameters
!
      integer(kind = kint) :: iele, icou
!
!
      icou = 0
      do iele = 1, ele1%numele
        if(iflag_refine_ele(iele) .eq. iflag_tri_full) then
          icou = icou + 1
          iele_tri(icou) = iele
        end if
      end do
!
      end subroutine set_triple_refine_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module set_element_refine_flag
