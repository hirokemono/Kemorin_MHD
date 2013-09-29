!
!      module mesh_interpolation
!
      module mesh_interpolation
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
        implicit none
!
!      subroutine interpolation_4_mesh_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolation_4_mesh_test
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_table
      use m_interpolated_geometry
      use interpolate_position
      use m_read_mesh_data
!
      use copy_itp_geometry_2_IO
      use write_interpolated_node
!
!     return global node from table
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_global_node'
      call s_interpolate_global_node
!
!     interpolate 2nd mesh from 1st mesh
!
      if (iflag_debug.eq.1)   write(*,*) 's_interpolate_position'
      call s_interpolate_position
!
!
      if (my_rank .lt. ndomain_dest) then
        if (iflag_debug.gt.0)  write(*,*) 's_copy_itp_geometry_2_IO'
        call s_copy_itp_geometry_2_IO(my_rank)
!
        if (iflag_debug.eq.1)                                           &
     &        write(*,*) 's_write_interpolate_node_file'
        call s_write_interpolate_node_file(my_rank)
!
      end if
!
      end subroutine interpolation_4_mesh_test
!
! ----------------------------------------------------------------------
!
      end module mesh_interpolation
