!
! ----- program  correlation_4_plane
!
      program    correlation_4_plane 
!
!    constract spectr data from simulation results 
!     By H. Matsui
!
!
      use m_precision
!
      use m_correlate_4_plane
!
      use t_size_of_cube
      use t_phys_data
      use t_time_data
      use t_ucd_data
      use t_mesh_data_4_merge
      use t_ctl_data_plane_correlate
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
      integer (kind = kint) :: iz
!
!  ===========
! . for local 
!  ===========

      type(ctl_data_plane_correlate), save :: pcor_c1
      type(size_of_cube), save :: c_size1
      type(phys_data), save :: cor_phys
      type(phys_data), save :: ref_phys

!>      Structure for time data
      type(time_data), save :: plane_t_IO
      type(ucd_data), save :: plane_ucd
!
      type(merged_mesh), save :: mgd_mesh_pm
      type(second_mesh), save :: sec_mesh_pm

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
      write(*,*) 'read_control_data_cor_plane'
      call read_control_data_cor_plane(pcor_c1)
!
      call set_ctl_params_correlate(pcor_c1, ist, ied, iint)
!
!     read outline of mesh
!
      call s_set_plane_size_correlate                                   &
     &   (pcor_c1%cube_c_corr, pcor_c1%cube2nd_c, c_size1,              &
     &    mgd_mesh_pm%num_pe, sec_mesh_pm%num_pe2)
!
      write(*,*) 'set_merged_node_and_element'
      call set_merged_node_and_element(cor_mesh_file, mgd_mesh_pm)
      write(*,*) 's_set_2nd_geometry_4_serial'
      call s_set_2nd_geometry_4_serial(ref_mesh_file, sec_mesh_pm)
!
      call s_set_numnod_4_plane(c_size1, mgd_mesh_pm%merge_tbl)
!
!   read field name and number of components
!
      call init_udt_4_correlate(ist, cor_phys, plane_t_IO, plane_ucd)
!
      call s_set_list_4_correlate                                       &
     &   (pcor_c1%fld_pc_ctl%field_ctl, ref_phys, cor_phys)
!
      write(*,*) 'internal_node, ele',                                  &
     &           mgd_mesh_pm%merge_tbl%inter_nod_m,                     &
     &           mgd_mesh_pm%merge_tbl%inter_ele_m
!
!     array allocation
!
      call allocate_correlate_4_plane
      call allocate_correlate_4_evo
!
!  open result file
!
       do iz = 1, c_size1%nz_all
        z_out(iz) = mgd_mesh_pm%merged%node%xx                          &
     &            (c_size1%nx_all*c_size1%ny_all*iz,3)
       end do
!
       call deallocate_ioverlap_nod
       call dealloc_node_geometry_w_sph(mgd_mesh_pm%merged%node)
       call dealloc_2nd_merge_table(sec_mesh_pm)
!
!
       call open_correlate_files_plane
!
!   loop for time integration
!
      do istep = ist, ied, iint
!
       write(*,*) 'read_udt_4_correlate'
       call read_udt_4_correlate                                        &
     &    (istep, mgd_mesh_pm, sec_mesh_pm, plane_t_IO, plane_ucd)
!
!  -------  Cross correlatiion
!
       write(*,*) 's_cal_x_correlate_4_plane'
       call s_cal_x_correlate_4_plane                                   &
     &         (istep, num_crt, num_domain_c, kx_max, ky_max, iz_max,   &
     &          phys_d1, ave_data, rms_data, sig_data,                  &
     &          phys_d2, ave_data2, rms_data2, sig_data2,               &
     &          crt_data, rms_ratio)
!
       write(*,*) 'step', istep, 'finish '
      end do

      call close_results_4_correlate
!
      stop ' //// program normally finished //// '

!
!
      end program correlation_4_plane 

!
