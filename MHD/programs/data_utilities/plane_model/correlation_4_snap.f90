!
! ----- program  correlation_4_snap
!
      program    correlation_4_snap
!
!    constract spectr data from simulation results 
!     By H. Matsui
!
!
      use m_precision
!
      use m_correlate_4_plane
      use m_size_4_plane
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_control_plane_correlate
!
      use t_phys_data
      use t_time_data
      use t_ucd_data
!

      use set_plane_size_correlate
      use set_numnod_4_plane
      use set_merged_geometry
      use set_geometry_to_merge
      use set_2nd_geometry_4_serial
      use read_udt_files_4_correlate
      use cal_x_correlate_4_plane
      use set_list_4_correlate

      implicit    none
!
      integer (kind = kint) :: ix, iy, iz
!
!  ===========
! . for local 
!  ===========

      type(phys_data), save :: cor_phys
      type(phys_data), save :: ref_phys

!>      Structure for time data
      type(time_data), save :: plane_t_IO
      type(ucd_data), save :: plane_ucd

      integer(kind=kint ) :: istep
      integer(kind=kint ) :: ist, ied, iint

! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================

      write(*,*) ' Dou you prepare folloing data???'
      write(*,*) ' control data for this routine:  ctl_corrlate'
      write(*,*) ' mesh data (default):  mesh/in.PE#'
      write(*,*) ' mesh data (default):  mesh_ref/in.PE#'
      write(*,*) ' field data (default): field/out.step#.PE#.udt'
      write(*,*) ' field data (default): field_ref/out.step#.PE#.udt'
      write(*,*) ' directory for spectr data: correlate.dat'
      write(*,*) ' hit return'
      read(*,*)

!  set parameters for results
!
      call read_control_data_cor_plane
!
      call set_ctl_params_correlate(ist, ied, iint)
!
!     read outline of mesh
!
      call s_set_plane_size_correlate(mgd_mesh1%num_pe, num_pe2)
!
      write(*,*) 'set_merged_node_and_element'
      call set_merged_node_and_element(cor_mesh_file)
      write(*,*) 's_set_2nd_geometry_4_serial'
!
      call s_set_2nd_geometry_4_serial(ref_mesh_file)
!
      call s_set_numnod_4_plane
!
!   read field name and number of components
!
      call init_udt_4_correlate(ist, cor_phys, plane_t_IO, plane_ucd)
!
       call s_set_list_4_correlate                                      &
     &    (fld_pc_ctl%field_ctl, ref_phys, cor_phys)
!
      write(*,*) 'internal_node, ele',                                  &
     &           merge_tbl%inter_nod_m,  merge_tbl%inter_ele_m
!
!     array allocation
!
      call allocate_correlate_4_plane
      call allocate_correlate_4_evo
!
!  open result file
!
       do ix = 1, nz_all
        x_out(ix) = mgd_mesh1%merged%node%xx(ix,1)
       end do
       do iy = 1, ny_all
        y_out(iy) = mgd_mesh1%merged%node%xx(nx_all*iy,2)
       end do
       do iz = 1, nz_all
        z_out(iz) = mgd_mesh1%merged%node%xx(nx_all*ny_all*iz,3)
       end do
!
       call deallocate_ioverlap_nod
       call deallocate_node_geometry_type(mgd_mesh1%merged%node)
       call deallocate_2nd_merge_table
!
!   loop for time integration
!
      do istep = ist, ied, iint
!
       call open_correlate_files_snap(istep)
!
       write(*,*) 'read_udt_4_correlate'
       call read_udt_4_correlate(istep, plane_t_IO, plane_ucd)
!
!  -------  Cross correlatiion
!
       write(*,*) 's_cal_x_correlate_4_snap'
       call s_cal_x_correlate_4_snap(istep)
!
      call close_results_4_correlate
!
      write(*,*) 'step', istep, 'finish '
      end do

      stop ' //// program normally finished //// '

!
!
      end program correlation_4_snap

!
