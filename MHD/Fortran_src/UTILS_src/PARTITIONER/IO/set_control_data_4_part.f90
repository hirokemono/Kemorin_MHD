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
      use itp_table_IO_select_4_zlib
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      if (mesh_file_prefix%iflag .gt. 0) then
        local_file_header = mesh_file_prefix%charavalue
      end if
!
      if (elem_file_prefix%iflag .gt. 0) then
        local_ele_header = elem_file_prefix%charavalue
      end if
!
      if (surf_file_prefix%iflag .gt. 0) then
        local_surf_header = surf_file_prefix%charavalue
      end if
!
      if (edge_file_prefix%iflag .gt. 0) then
        local_edge_header = edge_file_prefix%charavalue
      end if
!
!   set local data format
!
      call choose_file_format(org_mesh_file_fmt_ctl,                    &
     &    i_org_mesh_file_fmt, ifmt_single_mesh_fmt)
      call choose_file_format(mesh_file_fmt_ctl%charavalue,             &
     &    mesh_file_fmt_ctl%iflag, iflag_para_mesh_file_fmt)
!
!
      iflag_memory_conserve = 1
      if(memory_conservation_ctl%iflag .gt. 0                           &
     &  .and. no_flag(memory_conservation_ctl%charavalue)) then
        iflag_memory_conserve = 0
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
      if (ele_grp_ordering_ctl%icou .eq. 1) then
        nele_grp_ordering = ele_grp_ordering_ctl%num
!
        allocate(ele_grp_ordering(nele_grp_ordering))
        allocate(igrp_ele_ordering(nele_grp_ordering))
        ele_grp_ordering(1:nele_grp_ordering)                           &
     &      = ele_grp_ordering_ctl%c_tbl(1:nele_grp_ordering)
!
        call dealloc_ele_grp_ordering_ctl
      end if
!
      if (i_part_method .gt. 0) then
        if(      cmp_no_case(part_method_ctl, 'RCB')                    &
     &      .or. cmp_no_case(part_method_ctl, 'RCB_xyz')) then
            NTYP_div = iPART_RCB_XYZ
!
        else if( cmp_no_case(part_method_ctl, 'RCB_sph')) then
            NTYP_div = iPART_RCB_SPH
!
        else if( cmp_no_case(part_method_ctl, 'ES')                     &
     &      .or. cmp_no_case(part_method_ctl, 'ES_xyz') ) then
            NTYP_div = iPART_EQ_XYZ
!
        else if( cmp_no_case(part_method_ctl, 'ES_sph') ) then
            NTYP_div = iPART_EQ_SPH
!
        else if( cmp_no_case(part_method_ctl, 'ES_layered_sph')) then
            NTYP_div = iPART_LAYER_SPH
!
        else if( cmp_no_case(part_method_ctl, 'MeTiS_input')) then
            NTYP_div = iPART_GEN_MeTiS
!
        else if( cmp_no_case(part_method_ctl, 'MeTiS_RSB')) then
            NTYP_div = iPART_MeTiS_RSB
!
        else if( cmp_no_case(part_method_ctl, 'Cubed_sphere')           &
     &      .or. cmp_no_case(part_method_ctl, 'Cubed_sph')) then
            NTYP_div = iPART_CUBED_SPHERE
!
        else if( cmp_no_case(part_method_ctl, 'finer_mesh')             &
     &      .or. cmp_no_case(part_method_ctl, 'Divide_by_finer_mesh')   &
     &      ) then
            NTYP_div = iPART_FINE_MESH_TBL
!
        else if( cmp_no_case(part_method_ctl, 'Decomp_data')            &
     &      .or. cmp_no_case(part_method_ctl, 'decomposit_data')) then
            NTYP_div = iPART_FINE_MESH_TBL
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
        if(cmp_no_case(element_overlap_ctl, 'On')) then
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
        NPOWER_rcb = RCB_dir_ctl%num
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, RCB_dir_ctl%num
          if     (cmp_no_case(RCB_dir_ctl%c_tbl(i), 'X')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Y')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
          else if(cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Z')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. RCB_dir_ctl%num) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        call dealloc_num_bisection_ctl
!
      else if( NTYP_div .eq. iPART_RCB_SPH) then
        NPOWER_rcb = RCB_dir_ctl%num
        num_domain = 2**NPOWER_rcb
        allocate( idir_rcb(NPOWER_rcb) )
!
        NPOWER_rcb = 0
        do i = 1, RCB_dir_ctl%num
          if(      cmp_no_case(RCB_dir_ctl%c_tbl(i), 'R')               &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radius')          &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Radial')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 1
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Meridional')      &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Theta')           &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Elevation')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 2
!
          else if( cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Longitudinal')    &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Phi')             &
     &        .or. cmp_no_case(RCB_dir_ctl%c_tbl(i), 'Azimuth')) then
            NPOWER_rcb = NPOWER_rcb + 1
            idir_rcb(NPOWER_rcb) = 3
          end if
        end do
        if (NPOWER_rcb .lt. RCB_dir_ctl%num) then
          write(*,*) 'Set correct direction'
          stop
        end if
!
        call dealloc_num_bisection_ctl
!
        write(*,'(/,"### Spherical RECURSIVE COORDINATE BiSECTION")')
        write (*,*)  "number of level: ", NPOWER_rcb
        write (*,'(" Direction Code r:1, theta:2, phi:3")')
        write  (*,*) idir_rcb(1:NPOWER_rcb)
!
!
      else if (NTYP_div .eq. iPART_EQ_XYZ) then
        if(ndomain_section_ctl%num .ne. 3)                              &
     &         stop 'number of subdomain should be 3 directions'
!
        ndivide_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'X')             &
     &          ) ndivide_eb(1) = ndomain_section_ctl%ivec(i)
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Y')             &
     &          ) ndivide_eb(2) = ndomain_section_ctl%ivec(i)
          if(cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Z')             &
     &          ) ndivide_eb(3) = ndomain_section_ctl%ivec(i)
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
        call dealloc_num_subdomains_ctl
!
      else if (NTYP_div.eq.iPART_CUBED_SPHERE                           &
     &    .or. NTYP_div.eq.iPART_EQ_SPH                                 &
     &    .or. NTYP_div.eq.iPART_LAYER_SPH) then
        if(ndomain_section_ctl%num .ne. 3)                              &
     &         stop 'number of subdomain should be 3 directions'
!
        ndivide_eb(1:3) = 1
        do i = 1, ndomain_section_ctl%num
          if(      cmp_no_case(ndomain_section_ctl%c_tbl(i), 'R')       &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radius')  &
     &        .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Radial')  &
     &       ) then
            ndivide_eb(1) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Theta')   &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Meridional') &
     &     .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Elevation')  &
     &        ) then
            ndivide_eb(2) = ndomain_section_ctl%ivec(i)
!
          else if( cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Phi')     &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i),'Longitudinal') &
     &    .or. cmp_no_case(ndomain_section_ctl%c_tbl(i), 'Azimuth')     &
     &        ) then
            ndivide_eb(3) = ndomain_section_ctl%ivec(i)
          end if
        end do
        num_domain = ndivide_eb(1)*ndivide_eb(2)*ndivide_eb(3)
!
        call dealloc_num_subdomains_ctl
!
        if(NTYP_div .eq. iPART_LAYER_SPH) then
          num_egrp_layer = ele_grp_layering_ctl%num
          allocate(grp_layer_name(num_egrp_layer))
!
          if (num_egrp_layer .gt. 0) then
            grp_layer_name(1:num_egrp_layer)                            &
     &          = ele_grp_layering_ctl%c_tbl(1:num_egrp_layer)
          end if
          call dealloc_ele_grp_layer_ctl
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
          call choose_file_format(itp_tbl_format_ctl, i_fmt_itp_tbl,    &
     &        ifmt_itp_table_file)
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
