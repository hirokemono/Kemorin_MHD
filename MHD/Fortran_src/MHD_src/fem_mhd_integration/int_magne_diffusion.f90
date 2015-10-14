!
!      module int_magne_diffusion
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_int_magne_diffusion
!
      module int_magne_diffusion
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_magne_diffusion
!
      use m_control_parameter
      use m_geometry_data
      use m_int_vol_data
      use m_node_phys_address
      use m_node_phys_data
!
      use int_vol_vect_diff_1st
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
!  ---------  set number of integral points
!
       call reset_ff_smp(node1%max_nod_smp, f1_nl)
!
       call int_vol_rot_1st(ele1%istack_ele_smp, intg_point_poisson,    &
           iphys%i_vp_diffuse)
!
!      call cal_multi_pass_4_vector_ff
!      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,         &
!     &   ff, mhd_fem1_wk%ml_cd, nod_fld1%ntot_phys,                    &
!     &   iphys%i_magne, nod_fld1%d_fld)
       call cal_ff_smp_2_vector(node1, rhs_tbl1,                        &
     &     f1_nl%ff_smp, mhd_fem1_wk%ml_cd,                             &
     &     nod_fld1%ntot_phys, iphys%i_b_diffuse, nod_fld1%d_fld) 
!
       call vector_send_recv                                            &
     &    (nod_fld1%ntot_phys, iphys%i_b_diffuse, nod_fld1%d_fld)
!
      end subroutine s_int_magne_diffusion
!
! ----------------------------------------------------------------------
!
      end module int_magne_diffusion
