!
!      module set_list_4_correlate
!
!     Written by H. Matsui
!
!      subroutine set_ctl_params_correlate(ist, ied, iint)
!      subroutine s_set_list_4_correlate(ref_phys, cor_phys)
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
      subroutine set_ctl_params_correlate(ist, ied, iint)
!
      use m_control_plane_correlate
      use m_correlate_4_plane
      use m_ctl_data_4_time_steps
!
      integer(kind=kint ), intent(inout) :: ist, ied, iint
!
!
      ist = i_step_init_ctl
      ied = i_step_number_ctl
      iint = i_step_ucd_ctl
!
      cor_mesh_header = cor_mesh_head_ctl
      ref_mesh_header = ref_mesh_head_ctl
!
      cor_udt_header = cor_udt_head_ctl
      ref_udt_header = ref_udt_head_ctl
!
      end subroutine set_ctl_params_correlate
!
! -----------------------------------------------------------------------
!
      subroutine s_set_list_4_correlate(ref_phys, cor_phys)
!
      use m_correlate_4_plane
      use m_ctl_data_4_fields
!
      use t_phys_data
!
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
            do j = 1, num_nod_phys_ctl
              if (    phys_nod_name_ctl(j) .eq. cor_phys%phys_name(i)   &
     &          .and. visualize_ctl(j) .eq. 'Viz_On' ) then
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
            do j = 1, num_nod_phys_ctl
              if (    phys_nod_name_ctl(j) .eq. cor_phys%phys_name(i)   &
     &          .and. visualize_ctl(j) .eq. 'Viz_On' ) then
!
                do jj = 1, cor_phys%num_component(i)
                  crt_name(ii+jj-1) =   phys_nod_name_ctl(j)
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
