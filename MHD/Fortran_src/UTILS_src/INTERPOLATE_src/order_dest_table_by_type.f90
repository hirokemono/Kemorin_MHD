!>@file   order_dest_table_by_type.f90
!!@brief  module order_dest_table_by_type
!!
!!@author H. Matsui
!!@date  Programmed by H. Matsui in Sep., 2006
!
!>@brief Sort interpolation table by interpolation type
!!
!!@verbatim
!!      subroutine s_order_dest_table_by_type(node, ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!@endverbatim
!
      module order_dest_table_by_type
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_order_dest_table_by_type(node, ele)
!
      use calypso_mpi
!
      use t_geometry_data
!
      use m_geometry_constants
      use m_interpolate_table_dest
      use m_interpolate_coefs_dest
      use m_work_const_itp_table
!
      use count_interpolate_type_8
      use count_interpolate_type_20
      use count_interpolate_type_27
!
      use set_interpolate_type_8
      use set_interpolate_type_20
      use set_interpolate_type_27
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      integer(kind = kint) :: i, j, k, ist, ied, ist_type
!
!
      do i = 1, itp1_dest%num_org_domain
        ist = itp1_dest%istack_nod_tbl_dest(i-1) + 1
        ied = itp1_dest%istack_nod_tbl_dest(i)
        ist_type = 4*(i-1)
!
!        write(*,*) 'count', i, ist, ied
        if (ele%nnod_4_ele .eq. num_t_linear) then
          call s_count_interpolate_type_8(ist, ied,                     &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (ele%nnod_4_ele .eq. num_t_quad) then
          call s_count_interpolate_type_20(ist, ied,                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (ele%nnod_4_ele .eq. num_t_lag) then
          call s_count_interpolate_type_27(ist, ied,                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        end if
      end do
!
!
      call allocate_itp_coef_stack(itp1_dest%num_org_domain)
      istack_nod_tbl_wtype_dest(0) = 0
      do i = 1, itp1_dest%num_org_domain
        do j = 1, 4
          k = 4*(i-1) + j
          istack_nod_tbl_wtype_dest(k)                                  &
     &              = istack_nod_tbl_wtype_dest(k-1)                    &
     &               + nnod_table_wtype_dest(k)
        end do
      end do
!
      do i = 1, itp1_dest%num_org_domain
        ist = itp1_dest%istack_nod_tbl_dest(i-1) + 1
        ied = itp1_dest%istack_nod_tbl_dest(i)
        ist_type = 4*(i-1)
!
        if (ele%nnod_4_ele .eq. num_t_linear) then
          call s_order_interpolate_type_8(my_rank, ist, ied,            &
     &        istack_nod_tbl_wtype_dest(ist_type),                      &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (ele%nnod_4_ele .eq. num_t_quad) then
          call s_order_interpolate_type_20(my_rank, ist, ied,           &
     &        istack_nod_tbl_wtype_dest(ist_type),                      &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (ele%nnod_4_ele .eq. num_t_lag) then
          call s_order_interpolate_type_27(my_rank, ist, ied,           &
     &        istack_nod_tbl_wtype_dest(ist_type),                      &
     &        nnod_table_wtype_dest(ist_type+1) )
        end if
      end do
!
      call copy_table_2_order
!
      ntot_table_dest                                                   &
     &   = itp1_dest%istack_nod_tbl_dest(itp1_dest%num_org_domain)
!
      do i = 1, node%internal_node
        inod_gl_dest(i) = int(node%inod_global(inod_dest_4_dest(i)))
      end do
!
      end subroutine s_order_dest_table_by_type
!
!-----------------------------------------------------------------------
!
      end module order_dest_table_by_type
