!
!      module copy_itp_geometry_2_IO
!
      module copy_itp_geometry_2_IO
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
      implicit none
!
!      subroutine s_copy_itp_geometry_2_IO(my_rank)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_itp_geometry_2_IO(my_rank)
!
      use m_read_mesh_data
!
      use m_comm_data_IO
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_interpolated_geometry
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      write(*,*) 'num_neib_2', num_neib_2
      num_neib_domain_IO = num_neib_2
      call allocate_neib_domain_IO
!
      if (num_neib_2 .gt. 0) then
        id_neib_domain_IO(1:num_neib_2) = id_neib_2(1:num_neib_2)
      end if
!
      numnod_dummy = nnod_2nd
      call allocate_node_data_dummy
!
      globalnodid_dummy(1:nnod_2nd) = inod_global_itp(1:nnod_2nd)
      xx_dummy(1:nnod_2nd,1:3) = xx_interpolate(1:nnod_2nd,1:3)
!
      call deallocate_interpolate_geometry
!
!
      end subroutine s_copy_itp_geometry_2_IO
!
!------------------------------------------------------------------
!
      end module copy_itp_geometry_2_IO
