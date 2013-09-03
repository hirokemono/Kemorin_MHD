!
!      module set_control_data_4_part
!
      module set_control_data_4_part
!
!      Written by Kemorin on Sep. 2007
!
      use m_precision
!
      implicit    none
!
!      subroutine s_set_control_data_4_part
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_data_4_part
!
      use m_control_data_4_part
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
      use m_ctl_param_partitioner
      use m_partitioner_comm_table
      use read_sphere_surface
      use m_subdomain_table_IO
      use m_metis_IO
      use m_file_format_switch
!
      integer(kind = kint) :: i
!
!
      if (i_mesh_header .gt. 0) then
        local_file_header = mesh_file_prefix
      end if
!
      if (i_elem_header .gt. 0) then
        local_ele_header = elem_file_prefix
      end if
!
      if (i_surf_header .gt. 0) then
        local_surf_header = surf_file_prefix
      end if
!
      if (i_edge_header .gt. 0) then
        local_edge_header = edge_file_prefix
      end if
!
!   set local data format
!
      call choose_file_format(org_mesh_file_fmt_ctl,                    &
     &    i_org_mesh_file_fmt, ifmt_single_mesh_fmt)
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &    iflag_para_mesh_file_fmt)
!
!
      if(i_mem_conserve .gt. 0) then
        if(      memory_conservation_ctl .eq. 'no'                      &
     &      .or. memory_conservation_ctl .eq. 'No'                      &
     &      .or. memory_conservation_ctl .eq. 'NO'                      &
     &      .or. memory_conservation_ctl .eq. 'off'                     &
     &      .or. memory_conservation_ctl .eq. 'Off'                     &
     &      .or. memory_conservation_ctl .eq. 'OFF') then
          iflag_memory_conserve = 0
        end if
      end if
!
      write(*,*) 'iflag_memory_conserve', iflag_memory_conserve
!
!
      if (i_org_mesh_head .gt. 0) then
        global_mesh_head = org_mesh_head_ctl
      else
        write(*,*) 'Set original mesh data'
        stop
      end if
!
      write(*,*) 'i_part_method', i_part_method
!
      nele_grp_ordering = 0
      if (i_nele_grp_ordering .eq. 1) then
        nele_grp_ordering = nele_grp_ordering_ctl
!
        allocate(ele_grp_ordering(nele_grp_ordering))
        allocate(igrp_ele_ordering(nele_grp_ordering))
        ele_grp_ordering(1:nele_grp_ordering)                           &
     &      = ele_grp_ordering_ctl(1:nele_grp_ordering)
      end if
!
      if (i_part_method .gt. 0) then
        if      ( part_method_ctl .eq. 'RCB'                            &
     &       .or. part_method_ctl .eq. 'rcb'                            &
     &       .or. part_method_ctl .eq. 'RCB_xyz'                        &
     &       .or. part_method_ctl .eq. 'rcb_xyz') then
          NTYP_div = iPART_RCB_XYZ
        else if ( part_method_ctl .eq. 'RCB_sph'                        &
     &       .or. part_method_ctl .eq. 'rcb_sph') then
          NTYP_div = iPART_RCB_SPH
!
        else if ( part_method_ctl .eq. 'ES'                             &
     &       .or. part_method_ctl .eq. 'es'                             &
     &       .or. part_method_ctl .eq. 'ES_xyz'                         &
     &       .or. part_method_ctl .eq. 'es_xyz') then
          NTYP_div = iPART_EQ_XYZ
        else if ( part_method_ctl .eq. 'ES_sph'                         &
     &       .or. part_method_ctl .eq. 'es_sph') then
          NTYP_div = iPART_EQ_SPH
        else if ( part_method_ctl .eq. 'ES_layered_sph'                 &
     &       .or. part_method_ctl .eq. 'es_layered_sph') then
          NTYP_div = iPART_LAYER_SPH
!
        else if ( part_method_ctl .eq. 'MeTiS_input'                    &
     &       .or. part_method_ctl .eq. 'METIS_INPUT'                    &
     &       .or. part_method_ctl .eq. 'Metis_input'                    &
     &       .or. part_method_ctl .eq. 'metis_input') then
          NTYP_div = iPART_GEN_MeTiS
        else if ( part_method_ctl .eq. 'MeTiS_RSB'                      &
     &       .or. part_method_ctl .eq. 'METIS_RSB'                      &
     &       .or. part_method_ctl .eq. 'Metis_rsb'                      &
     &       .or. part_method_ctl .eq. 'metis_rsb') then
          NTYP_div = iPART_MeTiS_RSB
!
        else if ( part_method_ctl .eq. 'cubed_sphere'                   &
     &       .or. part_method_ctl .eq. 'Cubed_sphere'                   &
     &       .or. part_method_ctl .eq. 'cubed_sph'                      &
     &       .or. part_method_ctl .eq. 'Cubed_sph'                      &
     &       .or. part_method_ctl .eq. 'CUBED_SPH') then
          NTYP_div = iPART_CUBED_SPHERE
!
        else if ( part_method_ctl .eq. 'finer_mesh'                     &
     &       .or. part_method_ctl .eq. 'divide_by_finer_mesh'           &
     &       .or. part_method_ctl .eq. 'Divide_by_finer_mesh'           &
     &       .or. part_method_ctl .eq. 'Finer_mesh'                     &
     &       .or. part_method_ctl .eq. 'FINER_MESH') then
          NTYP_div = iPART_FINE_MESH_TBL
!
        else if ( part_method_ctl .eq. 'decomp_data'                    &
     &       .or. part_method_ctl .eq. 'decomposit_data'                &
     &       .or. part_method_ctl .eq. 'Decomposit_data'                &
     &       .or. part_method_ctl .eq. 'Decomp_data'                    &
     &       .or. part_method_ctl .eq. 'DECOMP_DATA') then
          NTYP_div = iPART_DECMP_MESH_TBL
        end if
!
        write (*,'(/," *********************************")')
        write (*,'(  " ||                             ||")')
        write (*,'(  " || GeoFEM Partitioner Ver.x.00 ||")')
        write (*,'(  " ||                  by Kemorin ||")')
        write (*,'(  " ||                             ||")')
        write (*,'(  " *********************************")')
        write (*,*) ' '
        write (*,'(/,"# PARTITIONING METHOD")')
        write (*,'(  "  RCB                                (1)")')
        write (*,'(  "  RCB(spherical corrdinate)          (2)")')
        write (*,'(  "  MeTiS/RSB                          (3)")')
        write (*,'(  "  generate MeTiS/RSB INPUT           (4)")')
        write (*,'(  "  Divide on sphere[cube base]        (5)")')
        write (*,'(  "  Divide by refereing finer grid     (6)")')
        write (*,'(  "  Divide using decomposition data    (7)")')
        write (*,'(  "  Divide equality with xyz direction (8)")')
        write (*,'(  "  Divide equality with rtp direction (9)")')
        write(*,*) 'decomposition code:', NTYP_div
!
      else
        write(*,*) 'Set partitioning method'
        stop
      end if
!
!
      if (i_sleeve_level .gt. 0) then
        n_overlap = sleeve_level_ctl
      else
        n_overlap = 1
      end if
      write(*,*) 'sleeve level :', n_overlap
!
      if (i_ele_overlap .gt. 0 .and. n_overlap .eq. 1) then
        if      ( element_overlap_ctl .eq. 'on'                         &
     &       .or. element_overlap_ctl .eq. 'On'                         &
     &       .or. element_overlap_ctl .eq. 'ON') then
          i_sleeve_ele = 1
          n_overlap =    2
        else
          i_sleeve_ele = 0
        end if
        write(*,*) 'element overlapping code: ', i_sleeve_ele
      end if
!
!
      if (NTYP_div .eq. iPART_RCB_XYZ) then
        NPOWER_rcb = num_RCB_level_ctl
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, num_RCB_level_ctl
          if      (RCB_dir_ctl(i) .eq. 'x'                              &
     &        .or. RCB_dir_ctl(i) .eq. 'X') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
          else if (RCB_dir_ctl(i) .eq. 'y'                              &
     &        .or. RCB_dir_ctl(i) .eq. 'Y') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
          else if (RCB_dir_ctl(i) .eq. 'z'                              &
     &        .or. RCB_dir_ctl(i) .eq. 'Z') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. num_RCB_level_ctl) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        deallocate(RCB_dir_ctl)
!
!
      else if( NTYP_div .eq. iPART_RCB_SPH) then
        NPOWER_rcb = num_RCB_level_ctl
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, num_RCB_level_ctl
          if      (RCB_dir_ctl(i) .eq. 'r'                              &
     &        .or. RCB_dir_ctl(i) .eq. 'R'                              &
     &        .or. RCB_dir_ctl(i) .eq. 'radius'                         &
     &        .or. RCB_dir_ctl(i) .eq. 'Radius'                         &
     &        .or. RCB_dir_ctl(i) .eq. 'RADIUS'                         &
     &        .or. RCB_dir_ctl(i) .eq. 'radial'                         &
     &        .or. RCB_dir_ctl(i) .eq. 'Radial'                         &
     &        .or. RCB_dir_ctl(i) .eq. 'RADIAL') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
          else if (RCB_dir_ctl(i) .eq. 'theta'                          &
     &        .or. RCB_dir_ctl(i) .eq. 'Theta'                          &
     &        .or. RCB_dir_ctl(i) .eq. 'THETA'                          &
     &        .or. RCB_dir_ctl(i) .eq. 'elevation'                      &
     &        .or. RCB_dir_ctl(i) .eq. 'Elevation'                      &
     &        .or. RCB_dir_ctl(i) .eq. 'ELEVATION') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
          else if (RCB_dir_ctl(i) .eq. 'phi'                            &
     &        .or. RCB_dir_ctl(i) .eq. 'Phi'                            &
     &        .or. RCB_dir_ctl(i) .eq. 'PHI'                            &
     &        .or. RCB_dir_ctl(i) .eq. 'azimuth'                        &
     &        .or. RCB_dir_ctl(i) .eq. 'Azimuth'                        &
     &        .or. RCB_dir_ctl(i) .eq. 'AZIMUTH') then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. num_RCB_level_ctl) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        deallocate(RCB_dir_ctl)
!
        write(*,'(/,"### Spherical RECURSIVE COORDINATE BiSECTION")')
        write (*,*)  "number of level: ", NPOWER_rcb
        write (*,'(" Direction Code r:1, theta:2, phi:3")')
        write  (*,*) idir_rcb(1:NPOWER_rcb)
!
!
      else if (NTYP_div .eq. iPART_EQ_XYZ) then
        ndivide_eb(1:3) = 1
        do i = 1, i_num_es
          if      (es_dir_ctl(i) .eq. 'x'                               &
     &        .or. es_dir_ctl(i) .eq. 'X') then
            ndivide_eb(1) = num_section_ctl(i)
          else if (es_dir_ctl(i) .eq. 'y'                               &
     &        .or. es_dir_ctl(i) .eq. 'Y') then
            ndivide_eb(2) = num_section_ctl(i)
          else if (es_dir_ctl(i) .eq. 'z'                               &
     &        .or. es_dir_ctl(i) .eq. 'Z') then
            ndivide_eb(3) = num_section_ctl(i)
          end if
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
      else if (NTYP_div.eq.iPART_CUBED_SPHERE                           &
     &    .or. NTYP_div.eq.iPART_EQ_SPH                                 &
     &    .or. NTYP_div.eq.iPART_LAYER_SPH) then
        ndivide_eb(1:3) = 1
        do i = 1, i_num_es
          if (es_dir_ctl(i) .eq. 'r'                                    &
     &        .or. es_dir_ctl(i) .eq. 'R'                               &
     &        .or. es_dir_ctl(i) .eq. 'radius'                          &
     &        .or. es_dir_ctl(i) .eq. 'Radius'                          &
     &        .or. es_dir_ctl(i) .eq. 'RADIUS'                          &
     &        .or. es_dir_ctl(i) .eq. 'radial'                          &
     &        .or. es_dir_ctl(i) .eq. 'Radial'                          &
     &        .or. es_dir_ctl(i) .eq. 'RADIAL') then
            ndivide_eb(1) = num_section_ctl(i)
          else if (es_dir_ctl(i) .eq. 'theta'                           &
     &        .or. es_dir_ctl(i) .eq. 'Theta'                           &
     &        .or. es_dir_ctl(i) .eq. 'THETA'                           &
     &        .or. es_dir_ctl(i) .eq. 'elevation'                       &
     &        .or. es_dir_ctl(i) .eq. 'Elevation'                       &
     &        .or. es_dir_ctl(i) .eq. 'ELEVATION') then
            ndivide_eb(2) = num_section_ctl(i)
          else if (es_dir_ctl(i) .eq. 'phi'                             &
     &        .or. es_dir_ctl(i) .eq. 'Phi'                             &
     &        .or. es_dir_ctl(i) .eq. 'PHI'                             &
     &        .or. es_dir_ctl(i) .eq. 'azimuth'                         &
     &        .or. es_dir_ctl(i) .eq. 'Azimuth'                         &
     &        .or. es_dir_ctl(i) .eq. 'AZIMUTH') then
            ndivide_eb(3) = num_section_ctl(i)
          end if
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
        if(NTYP_div .eq. iPART_LAYER_SPH) then
          num_egrp_layer = num_radial_layering_ctl
          allocate(grp_layer_name(num_egrp_layer))
!
          if (num_egrp_layer .gt. 0) then
            grp_layer_name(1:num_egrp_layer)                            &
     &          = ele_grp_layer_ctl(1:num_egrp_layer)
          end if
!
        else if ( NTYP_div .eq. iPART_CUBED_SPHERE) then
          if (i_sph_sf_file .eq. 1) then
            iflag_sphere_data = 1
            sphere_data_file_name = sphere_file_name_ctl
            write(*,*) 'Sphere surface correction file: ',              &
     &              trim(sphere_data_file_name)
          else
            iflag_sphere_data = 0
            write(*,*) 'No Sphere surface correction file '
          end if
        end if
!
!
      else if (NTYP_div .eq. iPART_DECMP_MESH_TBL                       &
     &    .or. NTYP_div .eq. iPART_FINE_MESH_TBL) then
        if (i_domain_tbl_file .eq. 1) then
          fname_subdomain = domain_group_file_ctl
        else
          write(*,*) 'set domain table file name'
          stop
        end if
!
        if(NTYP_div .eq. iPART_FINE_MESH_TBL) then
          if (i_fine_itp_file .eq. 1) then
            finer_inter_file_head = itp_tbl_head_ctl
          else
            write(*,*) 'set interpolate file name'
            stop
          end if
          if (i_fine_mesh_file .eq. 1) then
            finer_mesh_file_head = finer_mesh_head_ctl
          else
            write(*,*) 'set reference mesh file name'
            stop
          end if
        end if
!
      else if(NTYP_div .eq. iPART_MeTiS_RSB) then
        if (i_metis_in_file .eq. 1) then
          metis_sdom_name = metis_domain_file_ctl
        else
          write(*,*) 'set MeTiS input file name'
          stop
        end if
!
      else if (iPART_GEN_MeTiS .eq. iPART_GEN_MeTiS) then
        if (i_metis_dom_file .eq. 1) then
          metis_file_name = metis_input_file_ctl
        else
          write(*,*) 'set MeTiS domain file name'
          stop
        end if
!
        write ( *,'(/,"generate MeTiS/RSB input")')
        write  ( *,'(/,"*** GRID  file   ", a)')  org_mesh_header
        write  ( *,'(/,"*** MeTiS/RSB input  ", a)')  metis_file_name
      end if
!
      end subroutine s_set_control_data_4_part
!
! -----------------------------------------------------------------------
!
      end module set_control_data_4_part
