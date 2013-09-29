!
!     module order_dest_table_by_type
!
      module order_dest_table_by_type
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
      implicit none
!
!      subroutine s_order_dest_table_by_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_order_dest_table_by_type
!
      use calypso_mpi
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_interpolate_table_dest
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
      integer(kind = kint) :: i, j, k, ist, ied, ist_type
!
!
      do i = 1, num_org_domain
!
        ist = istack_nod_table_dest(i-1) + 1
        ied = istack_nod_table_dest(i)
        ist_type = 4*(i-1)
!
!        write(*,*) 'count', i, ist, ied
        if (nnod_4_ele .eq. num_t_linear) then
          call s_count_interpolate_type_8(ist, ied,                     &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (nnod_4_ele .eq. num_t_quad) then
          call s_count_interpolate_type_20(ist, ied,                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (nnod_4_ele .eq. num_t_lag) then
          call s_count_interpolate_type_27(ist, ied,                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        end if
!
      end do
!
!
!      write(*,*) 'stack'
      istack_nod_table_wtype_dest(0) = 0
      do i = 1, num_org_domain
        do j = 1, 4
          k = 4*(i-1) + j
          istack_nod_table_wtype_dest(k)                                &
     &              = istack_nod_table_wtype_dest(k-1)                  &
     &               + nnod_table_wtype_dest(k)
        end do
      end do
!
!
!      write(*,*) 'set', istack_nod_table_wtype_dest
      do i = 1, num_org_domain
!
        ist = istack_nod_table_dest(i-1) + 1
        ied = istack_nod_table_dest(i)
        ist_type = 4*(i-1)
!
        if (nnod_4_ele .eq. num_t_linear) then
          call s_order_interpolate_type_8(my_rank, ist, ied,            &
     &        istack_nod_table_wtype_dest(ist_type),                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (nnod_4_ele .eq. num_t_quad) then
          call s_order_interpolate_type_20(my_rank, ist, ied,           &
     &        istack_nod_table_wtype_dest(ist_type),                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        else if (nnod_4_ele .eq. num_t_lag) then
          call s_order_interpolate_type_27(my_rank, ist, ied,           &
     &        istack_nod_table_wtype_dest(ist_type),                    &
     &        nnod_table_wtype_dest(ist_type+1) )
        end if
!
      end do
!
      call copy_table_2_order
!
      ntot_table_dest = istack_nod_table_dest(num_org_domain)
!
      do i = 1, internal_node
        inod_gl_dest(i) = globalnodid(inod_dest_4_dest(i))
      end do
!
      end subroutine s_order_dest_table_by_type
!
!-----------------------------------------------------------------------
!
      end module order_dest_table_by_type
