!
!      module set_list_4_correlate
!
!     Written by H. Matsui
!
!!      subroutine set_ctl_params_correlate(pcor_c, ist, ied, iint)
!!      subroutine s_set_list_4_correlate(field_ctl, ref_phys, cor_phys)
!!        type(ctl_data_plane_correlate), intent(in) :: pcor_c
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: cor_phys
!!        type(phys_data), intent(inout) :: ref_phys
!
      module set_list_4_correlate
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_params_correlate(pcor_c, ist, ied, iint)
!
      use m_default_file_prefix
      use t_ctl_data_plane_correlate
      use m_correlate_4_plane
      use set_control_platform_data
!
      type(ctl_data_plane_correlate), intent(in) :: pcor_c
      integer(kind=kint ), intent(inout) :: ist, ied, iint
!
      type(read_character_item) :: ucd_format_ctl
!
!
      ist = 1
      if(pcor_c%t_pc_ctl%i_step_init_ctl%iflag .gt. 0) then
        ist = pcor_c%t_pc_ctl%i_step_init_ctl%intvalue
      end if
!
      ied = 1
      if(pcor_c%t_pc_ctl%i_step_number_ctl%iflag .gt. 0) then
        ied = pcor_c%t_pc_ctl%i_step_number_ctl%intvalue
      end if
!
      iint = 1
      if (pcor_c%t_pc_ctl%i_step_ucd_ctl%iflag .gt. 0) then
        iint = pcor_c%t_pc_ctl%i_step_ucd_ctl%intvalue
      end if
!
      call set_file_control_params(def_mesh_file_head,                  &
     &    pcor_c%cor_mesh_head_ctl, pcor_c%cor_mesh_fmt_ctl,            &
     &    cor_mesh_file)
      call set_file_control_params(def_mesh_file_head,                  &
     &    pcor_c%ref_mesh_head_ctl, pcor_c%ref_mesh_fmt_ctl,            &
     &    ref_mesh_file)
!
      call set_parallel_file_ctl_params(cor_udt_header,                 &
     &    pcor_c%cor_udt_head_ctl, ucd_format_ctl, cor_ucd_param)
      call set_parallel_file_ctl_params(ref_udt_header,                 &
     &    pcor_c%ref_udt_head_ctl, ucd_format_ctl, ref_ucd_param)
!
      end subroutine set_ctl_params_correlate
!
! -----------------------------------------------------------------------
!
      subroutine s_set_list_4_correlate(field_ctl, ref_phys, cor_phys)
!
      use m_correlate_4_plane
!
      use t_read_control_arrays
      use t_phys_data
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: cor_phys
      type(phys_data), intent(inout) :: ref_phys
!
      integer (kind = kint) :: i, j, k, ii, jj, icomp, icomp2
!
!
      num_crt = 0
      do i = 1, cor_phys%num_phys
        do k = 1, ref_phys%num_phys
          if ( cor_phys%phys_name(i) .eq. ref_phys%phys_name(k) ) then
            do j = 1, field_ctl%num
              if(cmp_no_case(field_ctl%c1_tbl(j),cor_phys%phys_name(i)) &
     &          .or. cmp_no_case(field_ctl%c2_tbl(j), 'Viz_On')) then
                num_crt = num_crt + cor_phys%num_component(i)
                exit
              end if
            end do
            exit
          end if 
        end do
      end do
!
      call allocate_correlate_name
!
      icomp = 1
      ii = 1
      do i = 1, cor_phys%num_phys
        icomp2 = 1
        do k = 1, ref_phys%num_phys
          if ( cor_phys%phys_name(i) .eq. ref_phys%phys_name(k) ) then
            do j = 1, field_ctl%num
              if(cmp_no_case(field_ctl%c1_tbl(j),cor_phys%phys_name(i)) &
     &          .or. cmp_no_case(field_ctl%c2_tbl(j), 'Viz_On')) then
                do jj = 1, cor_phys%num_component(i)
                  crt_name(ii+jj-1) =   field_ctl%c1_tbl(j)
                  ifield_crt(ii+jj-1) = icomp
                  ifield_crt2(ii+jj-1) = icomp2
                  icomp_crt(ii+jj-1) = jj-1
                end do
!
                if ( cor_phys%num_component(i) .eq. 1) then
                  crt_comp(ii) = 'scalar'
                  ii = ii + 1
                else if ( cor_phys%num_component(i) .eq. 3) then
                  crt_comp(ii  ) = 'x'
                  crt_comp(ii+1) = 'y'
                  crt_comp(ii+2) = 'z'
                  ii = ii + 3
                else if ( cor_phys%num_component(i) .eq. 6) then
                  crt_comp(ii  ) = 'xx'
                  crt_comp(ii+1) = 'xy'
                  crt_comp(ii+2) = 'xz'
                  crt_comp(ii+3) = 'yy'
                  crt_comp(ii+4) = 'yz'
                  crt_comp(ii+5) = 'zz'
                  ii = ii + 6
                end if
              end if
            end do
          end if
          icomp2 = icomp2 + ref_phys%num_component(k)
        end do
        icomp = icomp + cor_phys%num_component(i)
      end do
!
      write(*,*) 'ifield_crt ', ifield_crt
      write(*,*) 'ifield_crt2', ifield_crt2
      write(*,*) 'icomp_crt', icomp_crt
!
      end subroutine s_set_list_4_correlate
!
!  ---------------------------------------------------------------------
!
      end module set_list_4_correlate
