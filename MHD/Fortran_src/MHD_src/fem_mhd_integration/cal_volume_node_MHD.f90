!
!------- module cal_volume_node_MHD ---------------
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_volume_node(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module cal_volume_node_MHD
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_data
      use sum_volume_of_domain
!
      implicit none
!
      real(kind=kreal) :: vol_fl_local
      private :: vol_fl_local
!
      private :: cal_volume_4_fluid, cal_volume_4_conduct
      private :: cal_volume_4_insulate
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_node(layer_tbl)
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_jacobians
      use m_bulk_values
      use t_layering_ele_list
!
      use int_volume_of_domain
      use cal_layered_volumes
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call allocate_volume_4_smp
!
      if (iflag_debug.eq.1) write(*,*) 's_int_volume_of_domain'
      call s_int_volume_of_domain(ele1, jac1_3d_q)
!
!     ---  lead total volume of each area
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_fluid'
       call cal_volume_4_fluid
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_conduct'
       call cal_volume_4_conduct
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_insulate'
       call cal_volume_4_insulate
!
       if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (iflag_debug.eq.1) write(*,*) 's_cal_layered_volumes'
         call s_cal_layered_volumes(ele1, layer_tbl)
       end if
!
!
       if (iele_fl_smp_stack(np_smp) .eq. iele_fl_smp_stack(0)) then
         rms_local(ivol) = vol_local
       else
         rms_local(ivol) = vol_fl_local
       end if
!
!       call s_int_volume_insulate_core
!
       call deallocate_volume_4_smp
!
!
       if (iflag_debug.eq.1) then
         write(*,*) 'volume:       ', ele1%volume
         write(*,*) 'vol_fluid:    ', fluid1%volume
         write(*,*) 'vol_conduct:  ', conduct1%volume
         write(*,*) 'vol_insulate: ', insulate1%volume
       end if
!
       end subroutine cal_volume_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_fluid
!
      use m_geometry_data_MHD
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_fl_smp_stack, ele1%volume_ele, vol_fl_local)
!
      call MPI_allREDUCE (vol_fl_local, fluid1%volume, ione,            &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (fluid1%volume .eq. 0.0d0) then
        fluid1%a_volume = 1.0d30
      else
        fluid1%a_volume = 1.0d0 / fluid1%volume
      end if
!
      end subroutine cal_volume_4_fluid
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_conduct
!
      use m_geometry_data_MHD
!
      real(kind=kreal) :: vol_cd_local
!
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_cd_smp_stack, ele1%volume_ele, vol_cd_local)
!
      call MPI_allREDUCE (vol_cd_local, conduct1%volume, ione,          &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (conduct1%volume .eq. 0.0d0) then
        conduct1%a_volume = 1.0d30
      else
        conduct1%a_volume = 1.0d0 / conduct1%volume
      end if
!
      end subroutine cal_volume_4_conduct
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_insulate
!
      use m_geometry_data_MHD
!
      real(kind=kreal) :: vol_ins_local
!
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_ins_smp_stack, ele1%volume_ele, vol_ins_local)
!
      call MPI_allREDUCE (vol_ins_local, insulate1%volume, ione,        &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (insulate1%volume .eq. 0.0d0) then
        insulate1%a_volume = 1.0d30
      else
        insulate1%a_volume = 1.0d0 / insulate1%volume
      end if
!
      end subroutine cal_volume_4_insulate
!
!  ---------------------------------------------------------------------
!
      end module cal_volume_node_MHD
