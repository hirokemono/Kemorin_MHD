!
!     module transfer_field_2_rtp_grid
!
      module transfer_field_2_rtp_grid
!
!      Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: d_zonal(:,:)
      real(kind = kreal), allocatable :: vec_trans(:,:)
!
      private :: copy_phys_tmp_2_rtp, copy_rtp_phys_2_tmp
      private :: vec_trans
!
!      subroutine allocate_work_4_vect_trans
!      subroutine deallocate_work_4_vect_trans
!
!      subroutine copy_phys_name_to_rtp_phys
!
!      subroutine trans_nod_phys_2_rtp_phys(ifrag_trans_vect)
!      subroutine trans_rtp_phys_2_nod_phys(ifrag_trans_vect)
!
!      subroutine copy_nod_phys_2_rtp_phys(ist_comp, ied_comp)
!      subroutine copy_rtp_phys_2_nod_phys(ist_comp, ied_comp)
!
!      subroutine copy_zonal_phys_to_rtp
!      subroutine copy_zonal_phys_from_rtp
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_work_4_vect_trans
!
      use m_sph_spectr_data
      use m_geometry_parameter
!
      allocate( d_zonal(ntot_phys_rtp,numnod) )
      allocate( vec_trans(numnod,3) )
      d_zonal =   0.0d0
      vec_trans = 0.0d0
!
      end subroutine allocate_work_4_vect_trans
!
! -------------------------------------------------------------------
!
      subroutine deallocate_work_4_vect_trans
!
      deallocate( vec_trans )
!
      end subroutine deallocate_work_4_vect_trans
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_phys_name_to_rtp_phys
!
      use m_node_phys_data
      use m_sph_spectr_data
!
!
      num_phys_rtp =  num_nod_phys
      ntot_phys_rtp = num_tot_nod_phys
      call allocate_phys_rtp_name
!
      num_phys_comp_rtp(1:num_phys_rtp)                                 &
     &     = num_nod_component(1:num_phys_rtp)
      istack_phys_comp_rtp(0:num_phys_rtp)                              &
     &      = istack_nod_component(0:num_phys_rtp)
      phys_name_rtp(1:num_phys_rtp)                                     &
     &      = phys_nod_name(1:num_phys_rtp)
      iflag_monitor_rtp(1:num_phys_rtp)                                 &
     &      = iflag_nod_fld_monitor(1:num_phys_rtp)
!
      end subroutine copy_phys_name_to_rtp_phys
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine trans_nod_phys_2_rtp_phys(ifrag_trans_vect)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_vector_2_cyl_smp
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: ifrag_trans_vect
      integer(kind = kint) :: i, jcomp, i_field, jst, jed
!
!
      do i = 1, num_nod_phys
        i_field = istack_nod_component(i-1) + 1 
        if (num_nod_component(i) .eq. 3) then
!
!$omp parallel
          if ( ifrag_trans_vect .eq. 1) then
            call cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &          d_nod(1,i_field), vec_trans, xx, radius, s_cylinder,    &
     &          a_radius, a_s_cylinder)
          else if ( ifrag_trans_vect .eq. 2) then
            call cvt_vector_2_cyl_smp(np_smp, numnod,                   &
     &          inod_smp_stack, d_nod(1,i_field), vec_trans, xx,        &
     &          s_cylinder, a_s_cylinder)
          else
            call copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,    &
     &          d_nod(1,i_field), vec_trans)
          end if
!$omp end parallel
!
          jcomp = istack_phys_comp_rtp(i-1) + 1
          call copy_phys_tmp_2_rtp(jcomp)
!
        else
          jst = istack_phys_comp_rtp(i-1) + 1
          jed = istack_phys_comp_rtp(i)
          call copy_nod_phys_2_rtp_phys(jst, jed)
        end if
      end do
!
      end subroutine trans_nod_phys_2_rtp_phys
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine trans_rtp_phys_2_nod_phys(ifrag_trans_vect)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use cvt_sph_vector_2_xyz_smp
      use cvt_cyl_vector_2_xyz_smp
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: ifrag_trans_vect
      integer(kind = kint) :: i, jcomp, i_field, jst, jed
!
!
      do i = 1, num_nod_phys
        i_field = istack_nod_component(i-1) + 1 
        if (num_nod_component(i) .eq. 3) then
!
          jcomp = istack_phys_comp_rtp(i-1) + 1
          call copy_rtp_phys_2_tmp(jcomp)
!
!$omp parallel
          if ( ifrag_trans_vect .eq. 1) then
            call cvt_sph_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack, &
     &          d_nod(1,i_field), vec_trans, colatitude, longitude)
          else if ( ifrag_trans_vect .eq. 2) then
           call cvt_cyl_vect_2_xyz_smp(np_smp, numnod, inod_smp_stack, &
     &          d_nod(1,i_field), vec_trans, longitude)
          else
            call copy_nod_vector_smp(np_smp, numnod, inod_smp_stack,    &
     &          vec_trans, d_nod(1,i_field) )
          end if
!$omp end parallel
!
        else
          jst = istack_phys_comp_rtp(i-1) + 1
          jed = istack_phys_comp_rtp(i)
          call copy_rtp_phys_2_nod_phys(jst, jed)
        end if
      end do
!
      end subroutine trans_rtp_phys_2_nod_phys
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_phys_2_rtp_phys(ist_comp, ied_comp)
!
      use m_machine_parameter
      use m_node_phys_data
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: ist_comp, ied_comp
      integer(kind = kint) :: ip, ist, ied, inod, i
!
!
!$omp parallel do private(ip,ist,ied,inod,i)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
!
        do i = ist_comp, ied_comp
!cdir noloopchg
          do inod = ist, ied
            d_zonal(i,inod) = d_nod(inod,i)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_nod_phys_2_rtp_phys
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_2_nod_phys(ist_comp, ied_comp)
!
      use m_machine_parameter
      use m_node_phys_data
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: ist_comp, ied_comp
      integer(kind = kint) :: ip, ist, ied, inod, i
!
!
!$omp parallel do private(ip,ist,ied,inod,i)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
!
        do i = ist_comp, ied_comp
!cdir noloopchg
          do inod = ist, ied
            d_nod(inod,i) = d_zonal(i,inod)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_phys_2_nod_phys
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_phys_tmp_2_rtp(i_field)
!
      use m_machine_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
!cdir noloopchg
        do inod = ist, ied
          d_zonal(i_field,  inod) = vec_trans(inod,1)
          d_zonal(i_field+1,inod) = vec_trans(inod,2)
          d_zonal(i_field+2,inod) = vec_trans(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_phys_tmp_2_rtp
!
! -------------------------------------------------------------------
!
      subroutine copy_rtp_phys_2_tmp(i_field)
!
      use m_machine_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
!cdir noloopchg
        do inod = ist, ied
          vec_trans(inod,1) = d_zonal(i_field,  inod)
          vec_trans(inod,2) = d_zonal(i_field+1,inod)
          vec_trans(inod,3) = d_zonal(i_field+2,inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_phys_2_tmp
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_zonal_phys_to_rtp
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint) :: nd
!
      do nd = 1, ntot_phys_rtp
        d_rtp(1:nnod_rtp,nd) = d_zonal(nd,1:nnod_rtp)
      end do
!
      end subroutine copy_zonal_phys_to_rtp
!
! -------------------------------------------------------------------
!
      subroutine copy_zonal_phys_from_rtp
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint) :: nd
!
      do nd = 1, ntot_phys_rtp
        d_zonal(1:nnod_rtp,nd) = d_rtp(1:nnod_rtp,nd)
      end do
!
      end subroutine copy_zonal_phys_from_rtp
!
! -------------------------------------------------------------------
!
      end module transfer_field_2_rtp_grid
